CREATE OR REPLACE PACKAGE ek_checkit_finance_api IS

   /***************************************************************************
    * Package comments
    * ----------------
    * Created By : OAPGARTH
    * Created    : 08/07/2015 16:30:15
    * Purpose :
    *
    *    To serve requirements of CheckIt e-commerce orders (referred to in IFS
    *    as "Contracts").  Creation of a new Contract will trigger the creation
    *    of a manual voucher, which will represent the cost of parts associated
    *    with the contract (i.e. handheld-devices, probes etc.) spread over the
    *    lifetime of the contract.
    *
    *    Important: in IFS terminology, this voucher is in no way linked to the
    *    sale or to an IFS Customer Order.  It is more useful to think of it as
    *    being a stand-alone object, solely intended to model the cost of parts
    *    which is necessary for tax-reporting purposes.
    *
    *    Moreover, note that creation of this voucher is to be triggered by the
    *    release of a shipment of a Customer Order (and not by the confirmation
    *    of the contract's creation, notified via the CheckIt interface).  This
    *    means that the function will have no external interface to the CheckIt
    *    website, and will instead form an integral part of the IFS workflow at
    *    Elektron. 
    *
    */

PROCEDURE Find_Period_X_Months_In_Future (
   future_period_year_      OUT NUMBER,
   future_period_period_    OUT NUMBER,
   user_group_              IN  VARCHAR2,
   no_of_months_in_future_  IN  NUMBER  DEFAULT 1 );

   FUNCTION Create_Period_Allocated_Vou (
      contract_     IN  VARCHAR2,
      part_no_      IN  VARCHAR2 ) RETURN VARCHAR2;

END ek_checkit_finance_api;
/
CREATE OR REPLACE PACKAGE BODY ek_checkit_finance_api IS

COMPANY_ CONSTANT VARCHAR2(5) := 'ETUK1';

PROCEDURE Find_Period_X_Months_In_Future (
   future_period_year_      OUT NUMBER,
   future_period_period_    OUT NUMBER,
   user_group_              IN  VARCHAR2,
   no_of_months_in_future_  IN  NUMBER  DEFAULT 1 )
IS
   CURSOR period_from_future IS
      SELECT acc_yr, acc_pr
      FROM   ( SELECT rank() OVER (ORDER BY (p.accounting_year*100 + p.accounting_period) ASC) -1 rk,
                      p.accounting_year acc_yr, p.accounting_period acc_pr
               FROM   accounting_period_tab p
               WHERE  p.company = COMPANY_
                 AND  p.year_end_period = 'ORDINARY'
                 AND  p.date_until > SYSDATE )
      WHERE  rk = no_of_months_in_future_;
BEGIN
   OPEN  period_from_future;
   FETCH period_from_future INTO future_period_year_, future_period_period_;
   CLOSE period_from_future;
   User_Group_Period_API.Is_Period_Open (COMPANY_, future_period_year_, future_period_period_, user_group_);
END Find_Period_X_Months_In_Future;
   

FUNCTION Create_Period_Allocated_Vou (
   contract_     IN  VARCHAR2, -- contract_no_, order_no_??
   part_no_      IN  VARCHAR2 ) RETURN VARCHAR2
IS

   -- Constants for Development purposes
   NR_OF_PERIODS_  CONSTANT NUMBER := 24;

   -- Constants
   VOU_DATE_         CONSTANT DATE                                 := sysdate;
   INVENT_ACCOUNT_   CONSTANT account.account%TYPE                 := 1450;
   USER_GROUP_       CONSTANT VARCHAR2(2)                          := 'SV';
   VOUCHER_TYPE_     CONSTANT voucher_type_tab.voucher_type%TYPE   := 'M';
   CURR_TYPE_        CONSTANT currency_type_tab.currency_type%TYPE := '1';
   CURR_CODE_        CONSTANT currency_code_tab.currency_code%TYPE := Company_Finance_API.Get_Currency_Code(COMPANY_); --'GBP';
   --FEE_CODE_       CONSTANT statutory_fee_tab.fee_code%TYPE      := 'S20.0';
   --TAX_PCT_        CONSTANT NUMBER := Statutory_Fee_API.Get_Percentage(COMPANY_,FEE_CODE_)/100;
   CURR_RATE_        CONSTANT NUMBER := Currency_Rate_API.Get_Currency_Rate (COMPANY_, CURR_CODE_, CURR_TYPE_, SYSDATE);
   CONV_FACTOR_      CONSTANT NUMBER := Currency_Rate_API.Get_Conv_Factor (COMPANY_, CURR_CODE_, CURR_TYPE_, SYSDATE);
   DIV_FACTOR_       CONSTANT NUMBER := CURR_RATE_ / CONV_FACTOR_; -- divide the original � amount by this to get the foreign equivalent

   -- Head data
   temp_voucher_no_     voucher_tab.voucher_no%TYPE;
   final_voucher_no_    voucher_tab.voucher_no%TYPE;
   acc_year_            voucher_tab.accounting_year%TYPE;
   acc_per_             voucher_tab.accounting_period%TYPE;
   vou_type_desc_       voucher_type_tab.description%TYPE;
   transfer_id_         voucher_tab.transfer_id%TYPE;
   vou_row_attr_        VARCHAR2(32000);
   top_level_part_cost_ NUMBER;

   -- Row data
   account_             account.account%TYPE;
   cost_group_          cost_bucket_posting_group_tab.posting_group_id%TYPE;
   line_amount_gbp_     NUMBER;
   curr_amount_         NUMBER;  -- this is the � equivalent in foreign currency

   -- Data required to spread cost allocation
   active_acc_yr_       voucher_tab.accounting_year%TYPE;
   active_acc_pr_       voucher_tab.accounting_period%TYPE;
   until_acc_yr_        voucher_tab.accounting_year%TYPE;
   until_acc_pr_        voucher_tab.accounting_period%TYPE;
   cumul_amount_        NUMBER;
   split_pct_           NUMBER;
   split_amount_        NUMBER;

   info_                VARCHAR2(2000);
   attr_                VARCHAR2(2000);
   objid_               ROWID;
   objver_              VARCHAR2(2000);


   CURSOR get_cost_breakdown IS
      SELECT cost_bucket_id, bucket_accum_cost
      FROM   part_cost_bucket_tab pcb
      WHERE  pcb.contract = contract_
        AND  pcb.part_no = part_no_
        AND  pcb.top_level_part_no = part_no_
        AND  pcb.cost_set = '1'
        AND  pcb.alternative_no = '*'
        AND  pcb.top_alternative_no = '*'
        AND  pcb.routing_alternative_no = '*'
        AND  pcb.top_routing_no = '*'
        AND  pcb.bucket_seq = 0
        AND  pcb.cost_bucket_id != 'SYS';

   CURSOR get_voucher_rows IS
      SELECT vr.row_no, vr.amount
      FROM   voucher_row vr
      WHERE  vr.company = COMPANY_
        AND  vr.voucher_type = VOUCHER_TYPE_
        AND  vr.voucher_no = final_voucher_no_
        AND  vr.accounting_year = acc_year_;

   PROCEDURE Create_Vou_Row
   IS
      vou_row_rec_ voucher_api.VoucherRowRecType;
   BEGIN
      vou_row_rec_.company                := COMPANY_;
      vou_row_rec_.voucher_type           := VOUCHER_TYPE_;
      vou_row_rec_.function_group         := VOUCHER_TYPE_;
      vou_row_rec_.voucher_no             := temp_voucher_no_;
      vou_row_rec_.accounting_year        := acc_year_;
      vou_row_rec_.accounting_period      := acc_per_;
      vou_row_rec_.codestring_rec.code_a  := account_;
      vou_row_rec_.codestring_rec.code_i  := 'CK';
      --vou_row_rec_.optional_code          := FEE_CODE_;
      --vou_row_rec_.tax_direction          := 'TAXRECEIVED'; --Tax_Direction_API.Decode('TAXRECEIVED');
      vou_row_rec_.currency_code          := CURR_CODE_;
      vou_row_rec_.currency_rate          := CURR_RATE_;
      vou_row_rec_.conversion_factor      := CONV_FACTOR_;
      IF line_amount_gbp_ < 0 THEN
         vou_row_rec_.debet_amount           := -line_amount_gbp_;
         vou_row_rec_.credit_amount          := null;
         vou_row_rec_.currency_debet_amount  := -curr_amount_;    -- credit and debit to be decided
         vou_row_rec_.currency_credit_amount := null;               -- debit and credit to be decided
      ELSE
         vou_row_rec_.debet_amount           := null;
         vou_row_rec_.credit_amount          := line_amount_gbp_;
         vou_row_rec_.currency_debet_amount  := null;
         vou_row_rec_.currency_credit_amount := curr_amount_;
      END IF;
      --vou_row_rec_.tax_amount             := line_amount_gbp_ * TAX_PCT_;
      --vou_row_rec_.currency_tax_amount    := vou_row_rec_.tax_amount / DIV_FACTOR_;
      vou_row_rec_.text                   := 'not sure if any text needs to go here';
      vou_row_rec_.trans_code             := 'MANUAL';
      vou_row_rec_.auto_tax_vou_entry     := 'FALSE';
      vou_row_rec_.transfer_id            := transfer_id_;
      Voucher_Row_API.Add_New_Row_ (vou_row_rec_, vou_row_attr_, create_project_conn_ => FALSE);
   END Create_Vou_Row;

BEGIN
   Client_SYS.Clear_Attr  (attr_);

   -----------------------------------------------------------------------
   -- Create Manual Voucher Header
   --
   User_Group_Period_API.Get_And_Validate_Period (acc_year_, acc_per_, COMPANY_, USER_GROUP_, VOU_DATE_);
   vou_type_desc_ := Voucher_Type_API.Get_Description (COMPANY_, VOUCHER_TYPE_);

   Client_SYS.Add_To_Attr ('COMPANY',               COMPANY_,       attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_DATE',          VOU_DATE_,      attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_NO',            0,              attr_);
   Client_SYS.Add_To_Attr ('USERID',                'IFSAPP',       attr_);
   Client_SYS.Add_To_Attr ('USER_GROUP',            USER_GROUP_,    attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_DATE',          VOU_DATE_,      attr_);
   Client_SYS.Add_To_Attr ('VOUCHER_TYPE',          VOUCHER_TYPE_,  attr_);
   Client_SYS.Add_To_Attr ('DESC_VOUCHER_TYPE',     vou_type_desc_, attr_);
   Client_SYS.Add_To_Attr ('DATE_REG',              SYSDATE,        attr_);
   Client_SYS.Add_To_Attr ('ACCOUNTING_YEAR',       acc_year_,      attr_);
   Client_SYS.Add_To_Attr ('ACCOUNTING_PERIOD',     acc_per_,       attr_);
   Client_SYS.Add_To_Attr ('ENTERED_BY_USER_GROUP', USER_GROUP_,    attr_);
   Client_SYS.Add_To_Attr ('USE_CORRECTION_ROWS',   'FALSE',        attr_);
   IF Authorize_Level_API.Encode(Voucher_Type_User_Group_API.Get_Authorize_Level(COMPANY_,acc_year_,USER_GROUP_,VOUCHER_TYPE_))='Approved' THEN
      Client_SYS.Add_To_Attr ('VOUCHER_STATUS', Voucher_Status_API.Decode('Confirmed'), attr_);
   ELSE
      Client_SYS.Add_To_Attr ('VOUCHER_STATUS', Voucher_Status_API.Decode('AwaitingApproval'), attr_);
   END IF;
   Client_SYS.Add_To_Attr ('AMOUNT_METHOD', Def_Amount_Method_API.Decode(Company_Finance_API.Get_Def_Amount_Method(COMPANY_)), attr_);
   Voucher_API.New__ (info_, objid_, objver_, attr_, 'DO');

   -- Head parameters required to create the rows
   temp_voucher_no_     := Client_SYS.Get_Item_Value('VOUCHER_NO', attr_);
   transfer_id_         := Client_SYS.Get_Item_Value ('TRANSFER_ID', attr_);
   top_level_part_cost_ := Part_Cost_API.Get_Total_Accum_Cost (contract_, part_no_, '1', '*', '*');

   -----------------------------------------------------------------------
   -- Voucher Rows -- loop per cost-element to break down the debit,
   --                 plus one row credit row to balance the voucher
   --
   line_amount_gbp_ := -top_level_part_cost_ *1000;
   curr_amount_     := line_amount_gbp_ / DIV_FACTOR_;
   account_         := INVENT_ACCOUNT_;
   Create_Vou_Row;

   FOR cb_ IN get_cost_breakdown LOOP
      line_amount_gbp_ := cb_.bucket_accum_cost *1000;
      curr_amount_     := line_amount_gbp_ / DIV_FACTOR_;
      cost_group_      := Cost_Bucket_API.Get_Posting_Group_Id (contract_, cb_.cost_bucket_id);
      Posting_Ctrl_Detail_API.Get_Code_Part_Value (account_, 'COST', COMPANY_, 'M24', 'A', SYSDATE, 'C91', cost_group_);
      Create_Vou_Row;
   END LOOP;

   -----------------------------------------------------------------------
   -- Complete the voucher with the following calls
   Voucher_API.Ready_To_Update__ (info_, objid_, objver_, attr_, 'DO');
   Voucher_API.Finalize_Manual_Voucher__ (final_voucher_no_, COMPANY_, VOUCHER_TYPE_, transfer_id_);


   -----------------------------------------------------------------------
   -- Then split the cost of lines over the correct number of periods...
   -- Assume 24 months for Development testing
   FOR vr_ IN get_voucher_rows LOOP

      Find_Period_X_Months_In_Future (until_acc_yr_, until_acc_pr_, USER_GROUP_, NR_OF_PERIODS_-1);
      line_amount_gbp_ := vr_.amount;
      cumul_amount_    := line_amount_gbp_;
      split_pct_       := 1 / NR_OF_PERIODS_;
      split_amount_    := Currency_Amount_API.Get_Rounded_Amount (COMPANY_, CURR_CODE_, line_amount_gbp_/NR_OF_PERIODS_);

      FOR p_ IN 0..NR_OF_PERIODS_-1 LOOP

         IF p_ = NR_OF_PERIODS_-1 THEN
            split_amount_ := cumul_amount_; -- last might be slightly different, due to rounding errors
         END IF;
         cumul_amount_ := cumul_amount_ - split_amount_;
         Find_Period_X_Months_In_Future (active_acc_yr_, active_acc_pr_, USER_GROUP_, p_);

         Client_SYS.Clear_Attr  (attr_);
         Client_SYS.Add_To_Attr ('COMPANY',         COMPANY_,          attr_);
         Client_SYS.Add_To_Attr ('VOUCHER_TYPE',    VOUCHER_TYPE_,     attr_);
         Client_SYS.Add_To_Attr ('VOUCHER_NO',      final_voucher_no_, attr_);
         Client_SYS.Add_To_Attr ('ACCOUNTING_YEAR', acc_year_,         attr_);
         Client_SYS.Add_To_Attr ('ROW_NO',          vr_.row_no,        attr_);
         Client_SYS.Add_To_Attr ('ALLOC_PERIOD',    active_acc_pr_,    attr_);
         Client_SYS.Add_To_Attr ('ALLOC_PERCENT',   split_pct_,        attr_);
         Client_SYS.Add_To_Attr ('ALLOC_AMOUNT',    split_amount_,     attr_);
         Client_SYS.Add_To_Attr ('ALLOC_YEAR',      active_acc_yr_,    attr_);
         Client_SYS.Add_To_Attr ('UNTIL_YEAR',      until_acc_yr_,     attr_);
         Client_SYS.Add_To_Attr ('UNTIL_PERIOD',    until_acc_pr_,     attr_);
         Client_SYS.Add_To_Attr ('USER_GROUP',      USER_GROUP_,       attr_);
         Client_SYS.Add_To_Attr ('ALLOC_VOU_TYPE',  'X',               attr_);
         Period_Allocation_API.New__ (info_, objid_, objver_, attr_, 'DO');

      END LOOP;

   END LOOP;

   RETURN final_voucher_no_;

END Create_Period_Allocated_Vou;

END ek_checkit_finance_api;
/