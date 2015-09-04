CREATE OR REPLACE PACKAGE ek_checkit_finance_api IS

   /***************************************************************************
    * Package comments
    * ----------------
    * Created By : OAPGARTH
    * Created    : 08/07/2015 16:30:15
    * Package Purpose :
    *
    *    To serve financial requirements of CheckIt's e-commerce workflow.  The
    *    physical devlivery of an item (be that a handset, a probe or whatever)
    *    triggers the creation of inventory movements in finance.  However, the
    *    requirement at Elektron is to spread the cost of these postings across
    *    several financial periods, reflecting the fact that it is not "really"
    *    the parts that are being sold, but rather the underlying cloud-service
    *    which they allow the customer to access. 
    *
    *    Althogh Period Allocation is standard functionality in IFS, it appears
    *    that it cannot be applied automatically through posting controls.  The
    *    job of the program, is to automate this period allocation to inventory
    *    movements in finance.
    *
    */

   PROCEDURE Create_Period_Allocated_Vou;

END ek_checkit_finance_api;
/
CREATE OR REPLACE PACKAGE BODY ek_checkit_finance_api IS

-- Global Defined Constants
COMPANY_                   CONSTANT VARCHAR2(5)  := Company_Site_API.Get_Company('ECKT2');
USER_GROUP_                CONSTANT VARCHAR2(2)  := 'SV';
DEFERRED_ACCOUNT_COST_     CONSTANT VARCHAR2(20) := '1705';
DEFERRED_ACCOUNT_REVENUE_  CONSTANT VARCHAR2(20) := '2905';
COST_FLAG_                 CONSTANT VARCHAR2(11) := '[type:COST]';
REVENUE_FLAG_              CONSTANT VARCHAR2(14) := '[type:REVENUE]';
TYPE_PEACE_OF_MIND_        CONSTANT VARCHAR2(13) := 'Peace of Mind';
TYPE_NO_TIES_              CONSTANT VARCHAR2(7)  := 'No Ties';

-- Characteristic Constants
CHR_TECH_CLASS_CO_         CONSTANT VARCHAR2(7)  := 'CKCO';
CHR_CONTRACT_TYPE_         CONSTANT VARCHAR2(13) := 'CONTRACT TYPE';
CHR_ORIGINAL_CO_           CONSTANT VARCHAR2(20) := 'ORIGINAL CO';
CHR_CONTRACT_DURATION_     CONSTANT VARCHAR2(20) := 'CONTRACT DUR';
CHR_SHIPMENT_DATE_         CONSTANT VARCHAR2(13) := 'SHIPMENT DATE';
CHR_CONTRACT_END_DT_       CONSTANT VARCHAR2(20) := 'CONTRACT END DT';
CHR_CONTRACT_END_PR_       CONSTANT VARCHAR2(20) := 'CONTRACT END PR';
CHR_INV_PART_LIFE_         CONSTANT VARCHAR2(3)  := 'THL';


/****************************************************
 * START Private helper functions
 **/
FUNCTION Get_Nr_Periods_Until (
   until_acc_yr_   IN NUMBER,
   until_acc_pr_   IN NUMBER ) RETURN NUMBER
IS
   period_nr_in_future_   NUMBER;
   CURSOR get_period_nr_in_future IS
      WITH get_period_number AS (
         SELECT rank() OVER (ORDER BY (p.accounting_year*100 + p.accounting_period) ASC) rk,
                p.accounting_year acc_yr, p.accounting_period acc_pr
         FROM   accounting_period_tab p
         WHERE  p.company = COMPANY_
           AND  p.year_end_period = 'ORDINARY'
           AND  p.date_until > SYSDATE )
      SELECT rk
      FROM   get_period_number
      WHERE  acc_yr = until_acc_yr_
        AND  acc_pr = until_acc_pr_;
BEGIN
   OPEN  get_period_nr_in_future;
   IF get_period_nr_in_future%NOTFOUND THEN
      CLOSE get_period_nr_in_future;
      Error_SYS.Record_General ('AccRul', 'INVALIDPERIOD: Period is either in the past, or has not been generated yet.');
   ELSE
      FETCH get_period_nr_in_future INTO period_nr_in_future_;
      CLOSE get_period_nr_in_future;
   END IF;
   RETURN period_nr_in_future_;
END Get_Nr_Periods_Until;

FUNCTION Get_Nr_Periods_Until (
   yp_key_   IN NUMBER ) RETURN NUMBER
IS
   acc_yr_  NUMBER := trunc(yp_key_/100);
   acc_pr_  NUMBER := mod(yp_key_,100);
BEGIN
   RETURN Get_Nr_Periods_Until (acc_yr_, acc_pr_);
END Get_Nr_Periods_Until;

PROCEDURE Add_Periods (
   future_period_year_      OUT NUMBER,
   future_period_period_    OUT NUMBER,
   no_of_months_in_future_  IN  NUMBER   DEFAULT 1,
   base_date_               IN  DATE     DEFAULT sysdate,
   u_group_                 IN  VARCHAR2 DEFAULT USER_GROUP_ )
IS
   CURSOR period_from_future IS
      WITH accounting_period_ranks AS (
         SELECT rank() OVER (ORDER BY (p.accounting_year*100 + p.accounting_period) ASC) rk,
                p.accounting_year acc_yr, p.accounting_period acc_pr
         FROM   accounting_period_tab p
         WHERE  p.company = COMPANY_
           AND  p.year_end_period = 'ORDINARY'
           AND  p.date_until > base_date_ )
      SELECT acc_yr, acc_pr
      FROM   accounting_period_ranks
      WHERE  rk = no_of_months_in_future_;
BEGIN
   -- when no_of_months_in_future_=1, we will return the current period
   OPEN  period_from_future;
   FETCH period_from_future INTO future_period_year_, future_period_period_;
   CLOSE period_from_future;
   User_Group_Period_API.Is_Period_Open (COMPANY_, future_period_year_, future_period_period_, u_group_);
END Add_Periods;

FUNCTION Add_Periods (
   no_of_periods_  IN NUMBER,
   base_date_      IN DATE  DEFAULT sysdate ) RETURN NUMBER
IS
   future_period_yr_  NUMBER;
   future_period_pr_  NUMBER;
BEGIN
   Add_Periods (future_period_yr_, future_period_pr_, no_of_periods_, base_date_);
   RETURN (future_period_yr_ * 100) + future_period_pr_;
END Add_Periods;

PROCEDURE Check_Period_Open (
   periods_in_future_  IN NUMBER )
IS
   dummy_yr_   NUMBER;
   dummy_pr_   NUMBER;
BEGIN
   Add_Periods (dummy_yr_, dummy_pr_, periods_in_future_);
END Check_Period_Open;



FUNCTION Get_Corresponding_9_Account (
   in_account_  IN  VARCHAR2 ) RETURN VARCHAR2
IS
   chr1_     VARCHAR2(1) := substr(in_account_,1,1);
   chr34_    VARCHAR2(2) := substr(in_account_,3,2);
BEGIN
   RETURN
      CASE in_account_
         WHEN DEFERRED_ACCOUNT_COST_    THEN DEFERRED_ACCOUNT_COST_
         WHEN DEFERRED_ACCOUNT_REVENUE_ THEN DEFERRED_ACCOUNT_REVENUE_
         WHEN '4530'                    THEN '4931'
         ELSE chr1_ || '9' || chr34_
      END;
END Get_Corresponding_9_Account;

FUNCTION Split_This_Voucher_Row (
   row_type_     IN VARCHAR2,
   posting_acc_  IN VARCHAR2,
   credit_amt_   IN NUMBER,
   debit_amt_    IN NUMBER ) RETURN BOOLEAN
IS
   rtn_   BOOLEAN := false;
BEGIN
   IF row_type_ = COST_FLAG_ THEN
      rtn_ := (posting_acc_ like '49%' AND credit_amt_ is null) OR (posting_acc_=DEFERRED_ACCOUNT_COST_ AND debit_amt_ IS null);
   ELSIF row_type_ = REVENUE_FLAG_ THEN
      rtn_ := (posting_acc_ like '39%' AND debit_amt_ is null) OR (posting_acc_=DEFERRED_ACCOUNT_REVENUE_ AND credit_amt_ IS null);
   END IF;
   RETURN rtn_;
END Split_This_Voucher_Row;

------------------------------
-- START Technical Specification Getters/Setters
FUNCTION Get_Tech_Spec_Alpha (
   technical_spec_no_    IN NUMBER,
   technical_attribute_  IN VARCHAR2 ) RETURN VARCHAR2
IS
   ts_value_  technical_spec_alphanum.value_text%TYPE;
   CURSOR get_ts_alpha IS
      SELECT tsa.value_text
      FROM   technical_spec_alphanum tsa
      WHERE  tsa.technical_class = CHR_TECH_CLASS_CO_
        AND  tsa.technical_spec_no = technical_spec_no_
        AND  tsa.attribute = technical_attribute_;
BEGIN
   OPEN  get_ts_alpha;
   FETCH get_ts_alpha INTO ts_value_;
   CLOSE get_ts_alpha;
   RETURN ts_value_;
END Get_Tech_Spec_Alpha;

FUNCTION Get_Tech_Spec_Num (
   technical_spec_no_    IN NUMBER,
   technical_attribute_  IN VARCHAR2 ) RETURN NUMBER
IS
   ts_value_  technical_spec_numeric.value_no%TYPE;
   CURSOR get_ts_num IS
      SELECT tsn.value_no
      FROM   technical_spec_numeric tsn
      WHERE  tsn.technical_class = CHR_TECH_CLASS_CO_
        AND  tsn.technical_spec_no = technical_spec_no_
        AND  tsn.attribute = technical_attribute_;
BEGIN
   OPEN  get_ts_num;
   FETCH get_ts_num INTO ts_value_;
   CLOSE get_ts_num;
   RETURN ts_value_;
END Get_Tech_Spec_Num;

FUNCTION Get_Tech_Spec_Date (
   technical_spec_no_    IN NUMBER,
   technical_attribute_  IN VARCHAR2 ) RETURN DATE
IS
   dt_chr_   VARCHAR2(10) := Get_Tech_Spec_Alpha (technical_spec_no_, technical_attribute_);
   dt_rtn_   DATE         := to_date (dt_chr_, 'YYYY-MM-DD');
BEGIN
   RETURN dt_rtn_;
END Get_Tech_Spec_Date;


PROCEDURE Set_Tech_Spec_Alpha (
   tech_spec_id_ IN NUMBER,
   chr_name_     IN VARCHAR2,
   value_text_   IN VARCHAR2,
   chr_class_    IN VARCHAR2 DEFAULT CHR_TECH_CLASS_CO_ )
IS
   attr_        VARCHAR2(2000);
   rundate_chr_ VARCHAR2(18) := to_char (sysdate, 'YYYY-MM-DD HH24:MI');
BEGIN
   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('VALUE_TEXT', value_text_, attr_);
   Client_SYS.Add_To_Attr ('INFO', 'Updated by period-allocation automation on ' || rundate_chr_, attr_);
   Technical_Spec_Alphanum_API.Modify (tech_spec_id_, chr_class_, chr_name_, attr_);
END Set_Tech_Spec_Alpha;

PROCEDURE Set_Tech_Spec_Num (
   tech_spec_id_ IN NUMBER,
   chr_name_     IN VARCHAR2,
   value_num_    IN NUMBER,
   chr_class_    IN VARCHAR2 DEFAULT CHR_TECH_CLASS_CO_ )
IS
   attr_        VARCHAR2(2000);
   rundate_chr_ VARCHAR2(18) := to_char (sysdate, 'YYYY-MM-DD HH24:MI');
BEGIN
   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('VALUE_NO', value_num_, attr_);
   Client_SYS.Add_To_Attr ('INFO', 'Updated by period-allocation automation on ' || rundate_chr_, attr_);
   Technical_Spec_Numeric_API.Modify (tech_spec_id_, chr_class_, chr_name_, attr_);
END Set_Tech_Spec_Num;

PROCEDURE Set_Tech_Spec_Date (
   tech_spec_id_ IN NUMBER,
   chr_name_     IN VARCHAR2,
   value_date_   IN DATE,
   chr_class_    IN VARCHAR2 DEFAULT CHR_TECH_CLASS_CO_ )
IS BEGIN
   Set_Tech_Spec_Alpha (tech_spec_id_, chr_name_, to_char(value_date_,'YYYY-MM-DD'), chr_class_);
END Set_Tech_Spec_Date;


FUNCTION Get_Inv_Part_Char_Numeric (
   contract_    IN  VARCHAR2,
   part_no_     IN  VARCHAR2,
   char_code_   IN  VARCHAR2 ) RETURN NUMBER
IS
   val_chr_    inventory_part_char_tab.attr_value%TYPE;
   CURSOR get_characteristic_val IS
      SELECT ipc.attr_value
      FROM   inventory_part_char_tab ipc
      WHERE  contract = contract_
        AND  part_no = part_no_
        AND  characteristic_code = char_code_;
BEGIN
   OPEN  get_characteristic_val;
   FETCH get_characteristic_val INTO val_chr_;
   CLOSE get_characteristic_val;
   RETURN to_number (val_chr_);
END Get_Inv_Part_Char_Numeric;

PROCEDURE Set_Contract_End_Chr (
   co_number_      IN VARCHAR2,
   new_ship_date_  IN DATE )
IS
   curr_tech_spec_id_    NUMBER := Technical_Object_Reference_API.Get_Technical_Spec_No ('CustomerOrder', 'ORDER_NO='||co_number_||'^');
   new_ship_date_chr_    VARCHAR2(10) := to_char (new_ship_date_, 'YYYY-MM-DD');
   contract_type_        VARCHAR2(20);
   orig_co_no_           customer_order.order_no%TYPE;
   nr_of_periods_        NUMBER;
   contract_end_dt_      DATE;
   end_period_           NUMBER;
   orig_tech_spec_id_    NUMBER;

BEGIN
   IF new_ship_date_ IS NOT null THEN

      contract_end_dt_ := Get_Tech_Spec_Date (curr_tech_spec_id_, CHR_CONTRACT_END_DT_);

      IF contract_end_dt_ IS null THEN

         contract_type_ := Get_Tech_Spec_Alpha (curr_tech_spec_id_, CHR_CONTRACT_TYPE_);
         orig_co_no_    := Get_Tech_Spec_Alpha (curr_tech_spec_id_, CHR_ORIGINAL_CO_);

         IF contract_type_ = TYPE_PEACE_OF_MIND_ THEN
            IF orig_co_no_ = 'ORIGINAL' THEN
               Set_Tech_Spec_Alpha (curr_tech_spec_id_, CHR_SHIPMENT_DATE_, new_ship_date_chr_);
               nr_of_periods_   := Get_Tech_Spec_Num (curr_tech_spec_id_, CHR_CONTRACT_DURATION_);
               contract_end_dt_ := add_months (new_ship_date_, nr_of_periods_) -1;
               end_period_      := Add_Periods (nr_of_periods_, new_ship_date_);
            ELSE
               Customer_Order_API.Exist (orig_co_no_);
               orig_tech_spec_id_ := Technical_Object_Reference_API.Get_Technical_Spec_No ('CustomerOrder', 'ORDER_NO='||orig_co_no_||'^');
               contract_end_dt_   := Get_Tech_Spec_Date (orig_tech_spec_id_, CHR_CONTRACT_END_DT_);
               end_period_        := Get_Tech_Spec_Num (orig_tech_spec_id_, CHR_CONTRACT_END_PR_);
            END IF;
            Set_Tech_Spec_Date (curr_tech_spec_id_, CHR_CONTRACT_END_DT_, contract_end_dt_);
            Set_Tech_Spec_Num (curr_tech_spec_id_, CHR_CONTRACT_END_PR_, end_period_);
         ELSIF contract_type_ = TYPE_NO_TIES_ THEN
            NULL;  -- do nothing
         END IF;

      END IF;

   END IF;

END Set_Contract_End_Chr;
-- END Technical Specification Getters/Setters
------------------------------

/* 
 * END Private helper functions
 ***************************************************/



PROCEDURE Create_Period_Allocated_Vou
IS

   -- Constants
   VOU_DATE_            CONSTANT DATE          := sysdate;
   SHIP_VOU_TYPE_       CONSTANT VARCHAR2(3)   := 'MPL';
   FIN_VOU_TYPE_        CONSTANT VARCHAR2(3)   := 'F';
   M_VOUCHER_TYPE_      CONSTANT VARCHAR2(1)   := 'M';
   X_VOUCHER_TYPE_      CONSTANT VARCHAR2(1)   := 'X';
   CURR_CODE_           CONSTANT currency_code_tab.currency_code%TYPE := Company_Finance_API.Get_Currency_Code(COMPANY_); --'GBP';

   -- Head data
   temp_voucher_no_     voucher_tab.voucher_no%TYPE;
   final_voucher_no_    voucher_tab.voucher_no%TYPE;
   acc_year_            voucher_tab.accounting_year%TYPE;
   acc_per_             voucher_tab.accounting_period%TYPE;
   vou_type_desc_       voucher_type_tab.description%TYPE;
   transfer_id_         voucher_tab.transfer_id%TYPE;
   vou_row_attr_        VARCHAR2(32000);

   -- Data required to spread cost allocation
   tech_spec_id_        technical_object_reference.technical_spec_no%TYPE;
   nr_of_periods_       NUMBER;
   active_acc_yr_       voucher_tab.accounting_year%TYPE;
   active_acc_pr_       voucher_tab.accounting_period%TYPE;
   contract_end_yp_     NUMBER;
   until_acc_yr_        voucher_tab.accounting_year%TYPE;
   until_acc_pr_        voucher_tab.accounting_period%TYPE;
   line_amount_gbp_     NUMBER;
   cumul_pct_           NUMBER;
   cumul_amount_        NUMBER;
   split_pct_           NUMBER;
   split_amount_        NUMBER;

   info_                VARCHAR2(2000);
   attr_                VARCHAR2(2000);
   objid_               ROWID;
   objver_              VARCHAR2(2000);


   CURSOR get_ship_and_invoice_vr IS
      (
         SELECT vrc.company, 1 ord, COST_FLAG_ line_type, DEFERRED_ACCOUNT_COST_ defer_account,
                vrc.voucher_type, vrc.voucher_no, vrc.row_no, vrc.accounting_year, vrc.accounting_period,
                vrc.account, vrc.code_b, vrc.code_c, vrc.code_d, vrc.code_e, vrc.code_f, vrc.code_g, vrc.code_h, vrc.code_i, vrc.code_j,
                vrc.optional_code, vrc.tax_direction, vrc.currency_code, vrc.currency_rate, vrc.conversion_factor,
                vrc.debet_amount, vrc.credit_amount, vrc.currency_debet_amount, vrc.currency_credit_amount,
                vrc.reference_serie, vrc.reference_number, ith.date_applied, ith.contract, ith.part_no inv_part,
                col.order_no co_no, sp.catalog_group sales_group
         FROM   voucher_row vrc
                JOIN inventory_transaction_hist_tab ith
                     ON ith.transaction_id = vrc.mpccom_accounting_id
                JOIN customer_order_line_tab col
                     ON col.order_no = ith.order_no
                    AND col.line_no = ith.release_no
                    AND col.rel_no = ith.sequence_no
                    AND col.line_item_no = ith.line_item_no
                JOIN sales_part_tab sp
                     ON sp.contract = col.contract
                    AND sp.catalog_no = col.catalog_no
         WHERE  vrc.company = COMPANY_
           AND  vrc.voucher_type = SHIP_VOU_TYPE_
           AND  vrc.amount != 0
           AND  vrc.reference_serie = 'CUST ORDER'
           AND  sp.catalog_group IN ('CKPAH','CKCOH','CKCOA','CKCOP')
           AND  vrc.account IN ( SELECT pcd.code_part_value
                                 FROM   posting_ctrl_detail pcd
                                 WHERE  pcd.company = vrc.company
                                   AND  pcd.posting_type = 'M24'
                                   AND  pcd.code_part = 'A' )
           -- ensure we don't duplicate vouchers that have already been distributed
           AND  NOT EXISTS ( SELECT 1 FROM voucher_row_tab vr2
                             WHERE  vr2.company = vrc.company
                               AND  vr2.voucher_type = M_VOUCHER_TYPE_
                               AND  regexp_like (vr2.text, '\[orig-vou:'||vrc.voucher_type ||'-'||vrc.voucher_no||'-'||vrc.row_no ||'\]')
                           )
      )
      UNION
      (
         SELECT vrs.company, 2 ord, REVENUE_FLAG_ line_type, DEFERRED_ACCOUNT_REVENUE_ defer_account,
                vrs.voucher_type, vrs.voucher_no, vrs.row_no, vrs.accounting_year, vrs.accounting_period,
                vrs.account, vrs.code_b, vrs.code_c, vrs.code_d, vrs.code_e, vrs.code_f, vrs.code_g, vrs.code_h, vrs.code_i, vrs.code_j,
                vrs.optional_code, vrs.tax_direction, vrs.currency_code, vrs.currency_rate, vrs.conversion_factor,
                vrs.debet_amount, vrs.credit_amount, vrs.currency_debet_amount, vrs.currency_credit_amount,
                vrs.reference_serie, vrs.reference_number, null date_applied, sp.contract, sp.part_no inv_part,
                coi.order_no co_no, sp.catalog_group sales_group
         FROM   voucher_row vrs
                JOIN customer_order_inv_head ih
                     ON vrs.company = ih.company
                    AND vrs.reference_serie = ih.series_id
                    AND vrs.reference_number = ih.invoice_no
                JOIN customer_order_inv_item coi
                     ON ih.company = coi.company
                    AND ih.invoice_id = coi.invoice_id
                    AND vrs.item_id = coi.item_id
                JOIN sales_part_tab sp
                     ON sp.contract = coi.contract
                    AND sp.catalog_no = coi.catalog_no
         WHERE  vrs.company = COMPANY_
           AND  vrs.voucher_type = FIN_VOU_TYPE_
           AND  vrs.amount != 0
           AND  sp.catalog_group IN ('CKPAH')
           AND  vrs.account IN ( SELECT pcd.code_part_value
                                 FROM   posting_ctrl_detail pcd
                                 WHERE  pcd.company = vrs.company
                                   AND  pcd.posting_type = 'M28'
                                   AND  pcd.code_part = 'A' )
           -- ensure we don't duplicate vouchers that have already been distributed
           AND  NOT EXISTS ( SELECT 1 FROM voucher_row_tab vr3
                             WHERE  vr3.company = vrs.company
                               AND  vr3.voucher_type = M_VOUCHER_TYPE_
                               AND  regexp_like (vr3.text, '\[orig-vou:'||vrs.voucher_type ||'-'||vrs.voucher_no||'-'||vrs.row_no ||'\]')
                           )
      )
      ORDER BY ord ASC, co_no;

   dvr_       get_ship_and_invoice_vr%ROWTYPE;

   CURSOR get_rows_to_period_allocate IS
      SELECT vr.row_no, vr.amount, vr.text, regexp_replace (vr.text, '.*\[co:([^]]+)\].*', '\1') co_no --vr.reference_number co_no, vr.text
      FROM   voucher_row vr
      WHERE  vr.company = COMPANY_
        AND  vr.voucher_type = M_VOUCHER_TYPE_
        AND  vr.voucher_no = final_voucher_no_
        AND  vr.accounting_year = acc_year_
        AND  regexp_like (vr.text, '\[type:(COST|REVENUE)\]')
        AND  nvl(vr.amount,0) != 0;


   PROCEDURE Create_Vou_Row (
      orig_vr_       IN get_ship_and_invoice_vr%ROWTYPE,
      reverse_cost_  IN BOOLEAN )
   IS
      vou_row_rec_        voucher_api.VoucherRowRecType;
      posting_account_    account.ACCOUNT%TYPE;
      text_suffix_        VARCHAR2(200);
   BEGIN
      posting_account_ := Get_Corresponding_9_Account(orig_vr_.account);

      vou_row_rec_.company                := orig_vr_.company;
      vou_row_rec_.voucher_type           := M_VOUCHER_TYPE_;
      vou_row_rec_.function_group         := M_VOUCHER_TYPE_;
      vou_row_rec_.voucher_no             := temp_voucher_no_;
      vou_row_rec_.accounting_year        := orig_vr_.accounting_year;
      vou_row_rec_.accounting_period      := orig_vr_.accounting_period;
      vou_row_rec_.codestring_rec.code_a  := posting_account_;
      vou_row_rec_.codestring_rec.code_b  := orig_vr_.code_b;
      vou_row_rec_.codestring_rec.code_c  := orig_vr_.code_c;
      vou_row_rec_.codestring_rec.code_d  := orig_vr_.code_d;
      vou_row_rec_.codestring_rec.code_e  := orig_vr_.code_e;
      vou_row_rec_.codestring_rec.code_f  := orig_vr_.code_f;
      vou_row_rec_.codestring_rec.code_g  := orig_vr_.code_g;
      vou_row_rec_.codestring_rec.code_h  := orig_vr_.code_h;
      vou_row_rec_.codestring_rec.code_i  := orig_vr_.code_i;
      vou_row_rec_.codestring_rec.code_j  := orig_vr_.code_j;
      vou_row_rec_.optional_code          := orig_vr_.optional_code;
      vou_row_rec_.tax_direction          := orig_vr_.tax_direction; --'TAXRECEIVED'; --Tax_Direction_API.Decode('TAXRECEIVED');
      vou_row_rec_.currency_code          := orig_vr_.currency_code;
      vou_row_rec_.currency_rate          := orig_vr_.currency_rate;
      vou_row_rec_.conversion_factor      := orig_vr_.conversion_factor;
      IF reverse_cost_ THEN
         vou_row_rec_.debet_amount           := orig_vr_.credit_amount;
         vou_row_rec_.credit_amount          := orig_vr_.debet_amount;
         vou_row_rec_.currency_debet_amount  := orig_vr_.currency_credit_amount;
         vou_row_rec_.currency_credit_amount := orig_vr_.currency_debet_amount;
      ELSE
         vou_row_rec_.debet_amount           := orig_vr_.debet_amount;
         vou_row_rec_.credit_amount          := orig_vr_.credit_amount;
         vou_row_rec_.currency_debet_amount  := orig_vr_.currency_debet_amount;
         vou_row_rec_.currency_credit_amount := orig_vr_.currency_credit_amount;
      END IF;
      vou_row_rec_.reference_serie           := orig_vr_.reference_serie;
      vou_row_rec_.reference_number          := orig_vr_.reference_number;
      vou_row_rec_.trans_code                := 'MANUAL';
      vou_row_rec_.auto_tax_vou_entry        := 'FALSE';
      vou_row_rec_.transfer_id               := transfer_id_;

      IF Split_This_Voucher_Row (orig_vr_.line_type, posting_account_, vou_row_rec_.credit_amount, vou_row_rec_.debet_amount) THEN
         IF orig_vr_.sales_group IN ('CKCOH','CKCOA','CKCOP') THEN
            tech_spec_id_      := Technical_Object_Reference_API.Get_Technical_Spec_No ('CustomerOrder', 'ORDER_NO='||orig_vr_.co_no||'^');
            contract_end_yp_   := Get_Tech_Spec_Num (tech_spec_id_, CHR_CONTRACT_END_PR_);
            nr_of_periods_     := Get_Nr_Periods_Until (contract_end_yp_);
         ELSIF orig_vr_.sales_group IN ('CKPAH') THEN
            nr_of_periods_ := Get_Inv_Part_Char_Numeric (orig_vr_.contract, orig_vr_.inv_part, CHR_INV_PART_LIFE_);
         END IF;
         Check_Period_Open (nr_of_periods_);
         text_suffix_ := '[spread:' || nr_of_periods_ || ']';
      END IF;

      vou_row_rec_.text  := '[orig-vou:' || orig_vr_.voucher_type || '-' || orig_vr_.voucher_no || '-' || orig_vr_.row_no || ']' ||
                            '[site:' || orig_vr_.contract  || ';ip:' || orig_vr_.inv_part || ']' ||
                            '[sales-group:' || orig_vr_.sales_group || ']' ||
                            '[co:' || orig_vr_.co_no || ']' || orig_vr_.line_type || text_suffix_;
      Voucher_Row_API.Add_New_Row_ (vou_row_rec_, vou_row_attr_, create_project_conn_ => FALSE);
   END Create_Vou_Row;


BEGIN

   Client_SYS.Clear_Attr  (attr_);

   -----------------------------------------------------------------------
   -- Create Manual Voucher Header
   --
   User_Group_Period_API.Get_And_Validate_Period (acc_year_, acc_per_, COMPANY_, USER_GROUP_, VOU_DATE_);
   vou_type_desc_ := Voucher_Type_API.Get_Description (COMPANY_, M_VOUCHER_TYPE_);

   Client_SYS.Add_To_Attr ('COMPANY',               COMPANY_,        attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_DATE',          VOU_DATE_,       attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_NO',            0,               attr_);
   Client_SYS.Add_To_Attr ('USERID',                'IFSAPP',        attr_);
   Client_SYS.Add_To_Attr ('USER_GROUP',            USER_GROUP_,     attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_TYPE',          M_VOUCHER_TYPE_, attr_);
   Client_SYS.Add_To_Attr ('DESC_VOUCHER_TYPE',     vou_type_desc_,  attr_);
   Client_SYS.Add_To_Attr ('DATE_REG',              SYSDATE,         attr_);
   Client_SYS.Add_To_Attr ('ACCOUNTING_YEAR',       acc_year_,       attr_);
   Client_SYS.Add_To_Attr ('ACCOUNTING_PERIOD',     acc_per_,        attr_);
   Client_SYS.Add_To_Attr ('ENTERED_BY_USER_GROUP', USER_GROUP_,     attr_);
   Client_SYS.Add_To_Attr ('USE_CORRECTION_ROWS',   'FALSE',         attr_);
   IF Authorize_Level_API.Encode(Voucher_Type_User_Group_API.Get_Authorize_Level(COMPANY_,acc_year_,USER_GROUP_,M_VOUCHER_TYPE_))='Approved' THEN
      Client_SYS.Add_To_Attr ('VOUCHER_STATUS', Voucher_Status_API.Decode('Confirmed'), attr_);
   ELSE
      Client_SYS.Add_To_Attr ('VOUCHER_STATUS', Voucher_Status_API.Decode('AwaitingApproval'), attr_);
   END IF;
   Client_SYS.Add_To_Attr ('AMOUNT_METHOD', Def_Amount_Method_API.Decode(Company_Finance_API.Get_Def_Amount_Method(COMPANY_)), attr_);
   Voucher_API.New__ (info_, objid_, objver_, attr_, 'DO');

   -- Head parameters required to create the rows
   temp_voucher_no_     := Client_SYS.Get_Item_Value ('VOUCHER_NO', attr_);
   transfer_id_         := Client_SYS.Get_Item_Value ('TRANSFER_ID', attr_);


   -----------------------------------------------------------------------
   -- Create Voucher Rows as follows:
   --
   --   Loop for each existing voucher-row which has been posted to an inventory-costing account
   --   (i.e. 4xxx accounts) from inventory-transaction-history via posting control M24, control
   --   C91. Then for each row:
   --
   --    1 => create a new manual voucher line to shift the cost of inventory transactrions onto
   --         corresponding 49xx accounts
   --    2 => create a second manual voucher line to re-reverse the amount against corresponding
   --         49xx accounts.  (This line will later be period allocated so that reports show the
   --         the cost being transferred back to inventory over time.)
   --    3 => create a third manual voucher line to shift the burden of the inventory-cost onto a
   --         deferral account
   --    4 => and finally, a fourth voucher line which will (periodically) replenish the deferral
   --         account.  (Again, the period-allocation part is done later in the program.)
   --
   --   Functional Explanation:
   --
   --   Points 1 and 3 above shift the cost of the inventory transactions away from inventory and
   --   onto a theoretical "futures" or "deferral" account.  Doing this, shifts the burden of tax
   --   into the future, aligning with IR rules.
   --
   --   At first sight, points 2 and 4 appear to shift the cost-burden directly back to inventory
   --   again.  However, note that these are the lines that we are going to allocate periodically
   --   such that accounting reports will only show the cost being returned over a given duration
   --   of time.
   --
   --   Note that we do make sure here, to update the charactersistics of each Customer Order to
   --   include the correct "Contract End Date", via a call to: Set_Contract_End_Date_Chr()
   --
   FOR dvr_ IN get_ship_and_invoice_vr LOOP

      Set_Contract_End_Chr (dvr_.co_no, dvr_.date_applied);

      Create_Vou_Row (dvr_, true);
      Create_Vou_Row (dvr_, false);  -- this row will need to be period-allocated

      dvr_.account := dvr_.defer_account;
      Create_Vou_Row (dvr_, false);
      Create_Vou_Row (dvr_, true);  -- this row will need to be period-allocated

   END LOOP;


   -----------------------------------------------------------------------
   -- Complete the voucher with the following calls so that we obtain the final voucher number
   Voucher_API.Ready_To_Update__ (info_, objid_, objver_, attr_, 'DO');
   Voucher_API.Finalize_Manual_Voucher__ (final_voucher_no_, COMPANY_, M_VOUCHER_TYPE_, transfer_id_);


   -----------------------------------------------------------------------
   -- The objective of the following loop is to split the cost of the M-voucher lines which were
   -- created above, and period allocate them.  To find the correct allocation duration, we need
   -- to find the (effective) contract-end date, then calculate the number of periods from today
   -- to that date.  Two methods may be employed to get the effective contract-end-date.
   --
   --    a) When the order is linked to a contract, then we make use the the "Contract End Date"
   --       which we made sure to attach (above) to the Customer Order's Characteristics.
   --
   --    b) When the order is "No Ties" (or "Pay as you Go"), the concept of an end-date doesn't
   --       make much sense.  In this situation, we need to look at the inventory part, and find
   --       the effective lifespan (in number of months) on its associated characteristics.
   --
   FOR vr_ IN get_rows_to_period_allocate LOOP

      nr_of_periods_   := regexp_replace (vr_.text, '.*\[spread:(\d+)\].*', '\1');
      Add_Periods (until_acc_yr_, until_acc_pr_, nr_of_periods_);

      line_amount_gbp_ := vr_.amount;
      cumul_pct_       := 1;
      cumul_amount_    := line_amount_gbp_;
      split_pct_       := round (1/nr_of_periods_, 4);
      split_amount_    := Currency_Amount_API.Get_Rounded_Amount (COMPANY_, CURR_CODE_, line_amount_gbp_/nr_of_periods_);

      FOR p_ IN 1..nr_of_periods_ LOOP

         IF p_ = nr_of_periods_ THEN
            split_pct_    := cumul_pct_;
            split_amount_ := cumul_amount_;
         END IF;
         cumul_pct_    := cumul_pct_ - split_pct_;
         cumul_amount_ := cumul_amount_ - split_amount_;

         Add_Periods (active_acc_yr_, active_acc_pr_, p_);

         Client_SYS.Clear_Attr  (attr_);
         Client_SYS.Add_To_Attr ('COMPANY',         COMPANY_,          attr_);
         Client_SYS.Add_To_Attr ('VOUCHER_TYPE',    M_VOUCHER_TYPE_,   attr_);
         Client_SYS.Add_To_Attr ('VOUCHER_NO',      final_voucher_no_, attr_);
         Client_SYS.Add_To_Attr ('ACCOUNTING_YEAR', acc_year_,         attr_);
         Client_SYS.Add_To_Attr ('ROW_NO',          vr_.row_no,        attr_);
         Client_SYS.Add_To_Attr ('ALLOC_YEAR',      active_acc_yr_,    attr_);
         Client_SYS.Add_To_Attr ('ALLOC_PERIOD',    active_acc_pr_,    attr_);
         Client_SYS.Add_To_Attr ('ALLOC_PERCENT',   split_pct_,        attr_);
         Client_SYS.Add_To_Attr ('ALLOC_AMOUNT',    split_amount_,     attr_);
         Client_SYS.Add_To_Attr ('UNTIL_YEAR',      until_acc_yr_,     attr_);
         Client_SYS.Add_To_Attr ('UNTIL_PERIOD',    until_acc_pr_,     attr_);
         Client_SYS.Add_To_Attr ('USER_GROUP',      USER_GROUP_,       attr_);
         Client_SYS.Add_To_Attr ('ALLOC_VOU_TYPE',  X_VOUCHER_TYPE_,   attr_);
         Period_Allocation_API.New__ (info_, objid_, objver_, attr_, 'DO');

      END LOOP;

   END LOOP;

   Dbms_Output.Put_Line ('Final voucher created: ' || final_voucher_no_);

END Create_Period_Allocated_Vou;

END ek_checkit_finance_api;
/
