CREATE OR REPLACE PACKAGE EK_CUST_EVENT_UTIL_API IS
module_  CONSTANT VARCHAR2(25) := 'FNDBAS';
lu_name_ CONSTANT VARCHAR2(25) := 'FndEventAction';
-----------------------------------------------------------------------------
-------------------- PUBLIC DOMAIN METHODS ----------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-------------------- PUBLIC DOMAIN METHODS ----------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-------------------- PUBLIC TRANSLATION METHODS -----------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PROTECTED METHODS --------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
-----------------------------------------------------------------------------
FUNCTION ET_PARK_SO(SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2) RETURN NUMBER;
FUNCTION ET_TOOL_VALIDATE(SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2) RETURN NUMBER;

FUNCTION ET_ANALYSIS_ISSUES_EXIST(analysis_no IN VARCHAR2, Contract IN VARCHAR2) RETURN NUMBER;

PROCEDURE ET_DO_PARK (
    SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2);
PROCEDURE ET_DO_UNRESERVE (
    SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2, LINE_IT_NO IN VARCHAR2);
PROCEDURE ET_DO_PARK_NOW (
    passattr_ IN VARCHAR2 );
PROCEDURE ET_DO_UNRESERVE_NOW (
    passattr_ IN VARCHAR2 );
FUNCTION ET_II_COMMISSION_CHK(
     inv_id_     NUMBER, company_ varchar2 ) RETURN NUMBER;
FUNCTION ET_Tool_Label_print (   tool_rec_  CLOB,
                                        rec_num_   number ) RETURN VARCHAR2;

FUNCTION ET_GET_LOT_BATCH_SIZE(order_type IN VARCHAR2, order_no IN VARCHAR2, line_no IN NUMBER, release_no IN VARCHAR2, receipt_no IN NUMBER, sequence_no IN VARCHAR2, analysis_no IN VARCHAR2) RETURN NUMBER;

FUNCTION ET_NON_CONFORMS(analysis_no IN VARCHAR2) RETURN NUMBER;

FUNCTION ET_OUT_SPEC(analysis_no IN VARCHAR2) RETURN NUMBER;

PROCEDURE ET_DO_RELEASE_NOW (passattr_ IN VARCHAR2 );

PROCEDURE ET_SO_Release (SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2);

PROCEDURE ET_TOOL_UPDATED ( TOOL_INS_ID varchar2, contract_ varchar2);

PROCEDURE ET_RESERVATION_CHECK ( ATTR_ varchar2);

FUNCTION ET_GET_EMAIL_BY_WO(wo_no IN VARCHAR2, Contract IN VARCHAR2) RETURN VARCHAR2;
    
FUNCTION ET_GET_EMAIL_LIST(Group_Id IN VARCHAR2) RETURN VARCHAR2;

FUNCTION is_number (p_string IN VARCHAR2) RETURN NUMBER;
    
/*PROCEDURE ET_HANDLE_BLOCKED_CO (
   Ord_No_ IN VARCHAR2); */
-----------------------------------------------------------------------------
-------------------- FOUNDATION1 METHODS ------------------------------------
-----------------------------------------------------------------------------
PROCEDURE Init;
END EK_CUST_EVENT_UTIL_API;
/
CREATE OR REPLACE PACKAGE BODY EK_CUST_EVENT_UTIL_API IS
  -----------------------------------------------------------------------------
  -------------------- DOMAIN DECLARATIONS ------------------------------------
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- LU SPECIFIC IMPLEMENTATION METHOD DECLARATIONS ---------
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- PUBLIC DOMAIN METHODS ----------------------------------
  -----------------------------------------------------------------------------
  -- Exist
  --   Checks if given pointer (e.g. domain value) to an instance of this
  --   logical unit exists. If not an exception will be raised.
  --
  -- Enumerate
  --   Returns a list of all domain values.
  --
  -- Exist_DB
  --   Checks if an instance corresponding to a given database value exists.
  --   If not an exception will be raised.
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- PUBLIC DOMAIN METHODS ----------------------------------
  -----------------------------------------------------------------------------
  -- Encode
  --   Returns the stored database representation of a domain value given the
  --   client value in current language.
  --
  -- Decode
  --   Returns the client representation of a domain value in the
  --   current client language.
  --
  -- Get_Db_Value
  --   Returns the database representation a domain value given its index.
  --
  -- Get_Client_Value
  --   Returns the client representation a domain value given its index.
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- PUBLIC TRANSLATION METHODS -----------------------------
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- LU SPECIFIC PROTECTED METHODS --------------------------
  -----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  -------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
  -----------------------------------------------------------------------------
FUNCTION ET_PARK_SO(SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2) RETURN NUMBER IS
  Cursor get_D(so_id_ IN VARCHAR2, Release_No_ IN VARCHAR2, Sequence_No_ IN VARCHAR2) is
    select distinct sot.contract,
                    sot.order_no,
                    sot.release_no,
                    sot.sequence_no,
                    sot.tool_id,
                    nvl(pod.phase_out, to_date('31/12/2050', 'DD/MM/YYYY')) phase_out,
                    nvl(mtd.next_calibration_date,
                        to_date('31/12/2050', 'DD/MM/YYYY')) next_cal_date
      from ifsapp.SHOP_ORDER_OPER_TOOL sot,
           (select mt.contract, mt.tool_id, max(mt.end_date) phase_out
              from MANUF_TOOL_DETAIL_AVAIL mt
             group by mt.contract, mt.tool_id) pod,
           (select mtd.contract, mtd.tool_id, mtd.next_calibration_date
              from MANUF_TOOL_DETAIL mtd) mtd
     where ORDER_NO = so_id_ and RELEASE_NO = Release_No_ and SEQUENCE_NO = Sequence_No_
       and sot.contract = pod.contract
       and sot.tool_id = pod.tool_id
       and sot.contract = mtd.contract
       and sot.tool_id = mtd.tool_id;
  Phased_out_ number := 0;
  due_calib_  number := 0;
begin
  FOR i_ IN get_D(SO_ID,REL_NO,SEQ_NO) LOOP
    IF (i_.phase_out < sysdate) THEN
      Phased_out_ := Phased_out_ + 1;
    END IF;
    IF (i_.next_cal_date <= sysdate) THEN
      due_calib_ := due_calib_ + 1;
    END IF;
  END LOOP;
  IF (Phased_out_ > 0 or due_calib_ > 0) THEN
    RETURN 1;
  ELSE
    RETURN 0;
  END IF;
end ET_PARK_SO;
PROCEDURE ET_DO_PARK (
    SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2 ) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_           VARCHAR2(200);
BEGIN
 client_sys.Clear_Attr(attr_);
 client_sys.Add_To_Attr('SO_ID',SO_ID,attr_);
 client_sys.Add_To_Attr('REL_NO',REL_NO,attr_);
 client_sys.Add_To_Attr('SEQ_NO',SEQ_NO,attr_);
  Transaction_SYS.Deferred_Call('EK_CUST_EVENT_UTIL_API.ET_DO_PARK_NOW', attr_, 'Doing Parking SO');
/*INSERT INTO IFSAPP.EK_SO_PARK VALUES (SO_ID,REL_NO ,SEQ_NO );
commit;*/
--Error_Sys.Appl_General('ShopOrder', 'You cannot report time on a Shop Order having tools due for caliberation');
END ET_DO_PARK;
PROCEDURE ET_DO_UNRESERVE (
    SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2 , LINE_IT_NO IN VARCHAR2 ) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_           VARCHAR2(200);
BEGIN
Client_SYS.add_info('ShopOrder', 'You cannot issue items to a Shop Order having tools due for caliberation');
trace_sys.Message('TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT ET_DO_UNRESERVE 1');
 client_sys.Clear_Attr(attr_);
 client_sys.Add_To_Attr('SO_ID',SO_ID,attr_);
 client_sys.Add_To_Attr('REL_NO',REL_NO,attr_);
 client_sys.Add_To_Attr('SEQ_NO',SEQ_NO,attr_);
 client_sys.Add_To_Attr('LINE_IT_NO',LINE_IT_NO,attr_);
 Transaction_SYS.Deferred_Call('EK_CUST_EVENT_UTIL_API.ET_DO_UNRESERVE_NOW', attr_, 'Doing Unreserve SO');
END ET_DO_UNRESERVE;
PROCEDURE ET_DO_PARK_NOW (
   passattr_ IN VARCHAR2) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_      VARCHAR2(200);
   so_no VARCHAR2(25);
   rel_no VARCHAR2(25);
   SeQ_No VARCHAR2(25);
   CURSOR get_version(so_id_ IN VARCHAR2, Release_No_ IN VARCHAR2, Sequence_No_ IN VARCHAR2)  IS
      SELECT /*ROWID, ltrim(lpad(to_char(rowversion,'YYYYMMDDHH24MISS'),2000))*/ objid, objversion
      FROM  SHOP_ORD
      WHERE  order_no = so_id_
      AND   release_no = Release_No_
      AND   sequence_no = Sequence_No_;
BEGIN
so_no := Client_SYS.Get_Item_Value('SO_ID', passattr_);
rel_no := Client_SYS.Get_Item_Value('REL_NO', passattr_);
SeQ_No := Client_SYS.Get_Item_Value('SEQ_NO', passattr_);
   FOR i_ in get_version (so_no,rel_no,SeQ_No) LOOP
     trace_sys.Message('OBJID ----->'||i_.objid);
     IFSAPP.SHOP_ORD_API.Park__(info_,i_.objid,i_.objversion,attr_,'DO');
 END LOOP;
END ET_DO_PARK_NOW;
PROCEDURE ET_DO_UNRESERVE_NOW (
    passattr_ IN VARCHAR2 ) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_      VARCHAR2(200);
   so_no VARCHAR2(25);
   rel_no VARCHAR2(25);
   SeQ_No VARCHAR2(25);
   Line_It_No VARCHAR2(25);
BEGIN
so_no := Client_SYS.Get_Item_Value('SO_ID', passattr_);
rel_no := Client_SYS.Get_Item_Value('REL_NO', passattr_);
SeQ_No := Client_SYS.Get_Item_Value('SEQ_NO', passattr_);
Line_It_No := Client_SYS.Get_Item_Value('LINE_IT_NO', passattr_);
 IFSAPP.SHOP_MATERIAL_ALLOC_API.Unreserve(info_,attr_,so_no,rel_no,SeQ_No,Line_It_No);
END ET_DO_UNRESERVE_NOW;
FUNCTION ET_II_COMMISSION_CHK (   inv_id_     NUMBER, company_ varchar2
 ) RETURN  number IS
 CURSOR chk_invd is
 SELECT count(t.item_id) cnt from INV_ACCOUNTING_ROW_TAB t
 where t.company = company_ and t.invoice_id = inv_id_
 and ((t.code_a = 6050 and t.code_b = 615) or (t.code_a = 6050 and t.code_b = 610) or (t.code_a = 2902 ))
 group by company,invoice_id;
 ret_num_ number;
 Begin
 open chk_invd;
 fetch chk_invd into ret_num_;
 close chk_invd;
return nvl(ret_num_,0);
END ET_II_COMMISSION_CHK;
FUNCTION ET_Tool_Label_print (   tool_rec_  CLOB,
                                        rec_num_   number ) RETURN VARCHAR2 IS
  tool_list_ VARCHAR2(2000);
  ret_str_ VARCHAR2(2000);
  stmt_      VARCHAR2(2000);
  stmt1_     VARCHAR2(2000);
  stmt2_     VARCHAR2(2000);
  tool_      varchar2(100);
  --tool_rec_ CLOB;
  n_          number;
  i_          number;
  out_        number;
  rec_length_ number;
  init_len_   number;
begin
  n_ := 0;
  i_ := 0;
  init_len_   := 1;
  Delete EK_TOOL_PRINT_TAB;
  DELETE EK_TOOL_ID_PRINT_TAB;
  commit;
  Insert into EK_TOOL_PRINT_TAB values (tool_rec_);
  commit;
  --tool_rec_ := tool_rec1_;
  tool_list_ := chr(39);
  out_       := rec_num_;
  WHILE (out_ <> 0) LOOP
    --WHILE (rec_length_ <= 4000)  LOOP
    i_ := i_ + 1;
    stmt1_ := 'BEGIN select instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
              '-$0:' || '''' ||
              ',1,:i_) INTO :rec_length_ from ifsapp.EK_TOOL_PRINT_TAB t; END;';
    EXECUTE IMMEDIATE stmt1_
      USING  IN init_len_, IN i_, OUT rec_length_;
    if (rec_length_ = 0) then
      trace_sys.Message('Inside IF......... ' || to_char(i_));
      init_len_ := init_len_ + 4000;
      i_        := 1;
    end if;
    stmt_ := 'BEGIN select substr( substr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
             '-$0:' || '''' || ',1,:i_),' ||
             '((instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
             '-$1:' || '''' ||
             ',1,:i_))-(instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
             '-$0:' || '''' || ',1,:i_)))) ,' ||
             'instr(substr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
             '-$0:' || '''' || ',1,:i_),' ||
             '((instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
             '-$1:' || '''' ||
             ',1,:i_))-(instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
             '-$0:' || '''' || ',1,:i_)))),' || '''' || '=' || '''' ||
             ')+1) INTO :tool_ from ifsapp.EK_TOOL_PRINT_TAB t; END;';
    EXECUTE IMMEDIATE stmt_
      USING IN init_len_,IN i_,OUT tool_;
    trace_sys.Message('I less than ' || to_char(i_) ||
                      ' tool_ -------------->' || tool_ || '    ' ||
                      rec_length_);
    out_       := out_ - 1;
    insert into EK_TOOL_ID_PRINT_TAB values (tool_);
    commit;
    if (out_ > 0 ) then
       tool_list_ := tool_list_ || tool_ || chr(39) || chr(44) || chr(39);
    else
       tool_list_ := tool_list_ || tool_ || chr(39);
    --END LOOP;
    end if;
  END LOOP;
 -- tool_list_ := tool_list_ || chr(39);
  trace_sys.Message('tool_list_ -->' || tool_list_);
   stmt2_ := 'BEGIN select ifsapp.collect_func_long (cast (collect (s.tool_id) as ifsapp.mystringtabletypelong),'||''''||''''||''''||','||''''||''''||''''||')
    INTO :ret_str_ from (select distinct tool_id from ifsapp.EK_TOOL_ID_PRINT_TAB t) s; END;';
    EXECUTE IMMEDIATE stmt2_
      USING  IN  OUT ret_str_;
 trace_sys.Message('ret_str_ -->  ' || chr(39)||ret_str_||chr(39));
     return chr(39)||ret_str_||chr(39);
end ET_Tool_Label_print;
--get the lot batch size for purch or shop orders
FUNCTION ET_GET_LOT_BATCH_SIZE(order_type IN VARCHAR2, order_no VARCHAR2, line_no IN NUMBER, release_no IN VARCHAR2, receipt_no IN NUMBER, sequence_no IN VARCHAR2, analysis_no IN VARCHAR2) RETURN NUMBER IS
         Cursor get_purch_lot_size(order_no_ IN VARCHAR2, line_no_ IN NUMBER, release_no_ IN VARCHAR2, receipt_no_ IN NUMBER) is
         select t1.qty_arrived
                from IFSAPP.Purchase_Receipt t1 
                where t1.order_no = order_no_
                and t1.line_no = line_no_
                and t1.release_no = release_no_
                and t1.receipt_no = receipt_no_;
         Cursor get_shop_lot_size(order_no_ IN VARCHAR2, release_no_ IN VARCHAR2, sequence_no_ IN VARCHAR2) is
         select t2.revised_qty_due
                from IFSAPP.Shop_Ord t2 
                where t2.order_no = order_no_
                and t2.release_no = release_no_
                and t2.sequence_no = sequence_no_;
                
         Cursor get_sample_qty(analysis_no_ IN VARCHAR2) is
                select max(k.qty_to_control)
                from IFSAPP.QMAN_NORM_FOR_CONTROL_PLAN k 
                where  
                k.analysis_no = analysis_no_;
                                
         lot_size_  NUMBER;
begin
      if  order_type = 'PurchOrderAnalysis' then
          open get_purch_lot_size(order_no, line_no, release_no, receipt_no);
          fetch get_purch_lot_size into lot_size_;
          close get_purch_lot_size;
      else if order_type = 'InventoryAnalysis' then
          open get_sample_qty(analysis_no);
          fetch get_sample_qty into lot_size_;
          close get_sample_qty;        
      else
          open get_shop_lot_size(order_no, release_no, sequence_no);
          fetch get_shop_lot_size into lot_size_;
          close get_shop_lot_size;
      end if;
      end if;
      
      return nvl(lot_size_,0);        

end ET_GET_LOT_BATCH_SIZE;

--to check non conforms
FUNCTION ET_NON_CONFORMS(analysis_no IN VARCHAR2) RETURN NUMBER IS

         Cursor get_non_conforms(analysis_no_ IN VARCHAR2) is
                select sum(t.non_conform_qty)
                from IFSAPP.QMAN_NORM_FOR_CONTROL_PLAN t 
                where
                t.analysis_no = analysis_no_
                group by t.analysis_no;   
 ret_num_ number;
begin
 open get_non_conforms(analysis_no);
      fetch get_non_conforms into ret_num_;
 close get_non_conforms;
 return nvl(ret_num_,0);
   
end ET_NON_CONFORMS;

--no of out of spec 
FUNCTION ET_OUT_SPEC(analysis_no IN VARCHAR2) RETURN NUMBER IS

         Cursor get_out_spec(analysis_no_ IN VARCHAR2) is
                select count(*)
                from IFSAPP.QMAN_SAMPLE_VALUE h 
                where
                h.analysis_no = analysis_no_
                and h.result_status_db= 'OUT OF SPECIFICATION';

 ret_num_ number;
begin
 open get_out_spec(analysis_no);
      fetch get_out_spec into ret_num_;
 close get_out_spec;
 return nvl(ret_num_,0);
   
end ET_OUT_SPEC;

 

/*PROCEDURE ET_HANDLE_BLOCKED_CO (
   Ord_No_ IN VARCHAR2) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_      VARCHAR2(200);
   CURSOR get_lines(ord_no_ IN VARCHAR2)  IS
      SELECT  objid, objversion
      FROM  CUSTOMER_ORDER LINE col
      WHERE
      AND   order_no = Ord_No_;
BEGIN
\*  FOR i_ in   get_lines(ord_no_) LOOP
   client_sys.Clear_Attr(attr_);
   client_sys.Add_To_Attr('SO_ID',SO_ID,attr_);
  END LOOP;*\
  RETURN;
END ET_HANDLE_BLOCKED_CO;*/

FUNCTION ET_ANALYSIS_ISSUES_EXIST(analysis_no IN VARCHAR2, Contract IN VARCHAR2) RETURN NUMBER IS

         Cursor get_analysis(analysis_no_ IN VARCHAR2, Contract_ IN VARCHAR2) is
                select 1
                from IFSAPP.ANALYSIS_FULL t 
                where CONTROL_PLAN_NO IS NOT NULL 
                AND  CTRL_PLAN_REVISION_NO IS NOT NULL 
                AND t.analysis_no = analysis_no_
                AND t.contract = Contract_
                AND (t.attribute_result_status = 'Non Conformities Exists'
                OR t.variable_result_status in ('Out of Specification', 'Partly In Specification'))
                AND t.state = 'Confirmed'
                AND  CONTRACT IN (SELECT CONTRACT FROM IFSAPP.SITE_PUBLIC  WHERE CONTRACT = IFSAPP.User_Allowed_Site_API.Authorized(Contract_));   
 
begin
 
 FOR i_ IN get_analysis(analysis_no,Contract) LOOP
    RETURN 1;
 END LOOP;
 RETURN 0;

end ET_ANALYSIS_ISSUES_EXIST;


FUNCTION ET_TOOL_VALIDATE(SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2) RETURN NUMBER IS
  Cursor get_D(so_id_ IN VARCHAR2, Release_No_ IN VARCHAR2, Sequence_No_ IN VARCHAR2) is
select 1 from (select distinct 
                    s.revised_start_date start_date,
                    s.revised_due_date finish_date,
                    nvl(mtd.next_calibration_date,
                        to_date('31/12/2050', 'DD/MM/YYYY')) next_cal_date
      from ifsapp.SHOP_ORDER_OPER_TOOL sot,
      ifsapp.shop_ord s,
           (select mtd.contract, mtd.tool_id, mtd.next_calibration_date
              from MANUF_TOOL_DETAIL mtd) mtd
    where s.ORDER_NO = so_id_ and s.RELEASE_NO = Release_No_ and s.SEQUENCE_NO = Sequence_No_
       and sot.contract = mtd.contract
       and sot.tool_id = mtd.tool_id
       and sot.order_no = s.order_no
       and sot.sequence_no = s.sequence_no
       and sot.release_no = s.release_no
       and sot.contract = s.contract
       ) TT
       where  ( next_cal_date between   
       start_date and finish_date
       or next_cal_date <start_date);
  
  Phased_out_ number := 0;
  due_calib_  number := 0;
begin

 
 FOR i_ IN get_D(SO_ID,REL_NO,SEQ_NO) LOOP
    RETURN 1;
 END LOOP;
 RETURN 0;

end ET_TOOL_VALIDATE;


PROCEDURE ET_DO_RELEASE_NOW (
   passattr_ IN VARCHAR2) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_      VARCHAR2(200);
   so_no VARCHAR2(25);
   rel_no VARCHAR2(25);
   SeQ_No VARCHAR2(25);
   CURSOR get_version(so_id_ IN VARCHAR2, Release_No_ IN VARCHAR2, Sequence_No_ IN VARCHAR2)  IS
      SELECT /*ROWID, ltrim(lpad(to_char(rowversion,'YYYYMMDDHH24MISS'),2000))*/ objid, objversion
      FROM  SHOP_ORD
      WHERE  order_no = so_id_
      AND   release_no = Release_No_
      AND   sequence_no = Sequence_No_;
BEGIN
so_no := Client_SYS.Get_Item_Value('SO_ID', passattr_);
rel_no := Client_SYS.Get_Item_Value('REL_NO', passattr_);
SeQ_No := Client_SYS.Get_Item_Value('SEQ_NO', passattr_);
   FOR i_ in get_version (so_no,rel_no,SeQ_No) LOOP
     trace_sys.Message('OBJID ----->'||i_.objid);
     IFSAPP.SHOP_ORD_API.Release__(info_,i_.objid,i_.objversion,attr_,'DO');
 END LOOP;
END ET_DO_RELEASE_NOW;


PROCEDURE ET_SO_Release (
    SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2 ) IS
   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_           VARCHAR2(200);
   objversion_           VARCHAR2(200);
BEGIN
 client_sys.Clear_Attr(attr_);
 client_sys.Add_To_Attr('SO_ID',SO_ID,attr_);
 client_sys.Add_To_Attr('REL_NO',REL_NO,attr_);
 client_sys.Add_To_Attr('SEQ_NO',SEQ_NO,attr_);
 Transaction_SYS.Deferred_Call('EK_CUST_EVENT_UTIL_API.ET_DO_RELEASE_NOW', attr_, 'Release SO:'|| SO_ID);
END ET_SO_Release;

PROCEDURE ET_TOOL_UPDATED ( TOOL_INS_ID varchar2, contract_ varchar2) IS
  info_           VARCHAR2(2000);
     CURSOR get_parked_so(contract_ IN VARCHAR2)  IS
      SELECT  order_no, release_no, sequence_no
      FROM  SHOP_ORD
      WHERE  contract = contract_
      and order_no = '46863'
      AND   state = 'Parked';

BEGIN

      FOR i_ in get_parked_so (contract_) LOOP
         ET_SO_Release(i_.order_no, i_.release_no, i_.sequence_no);
      END LOOP;
 
END ET_TOOL_UPDATED;

PROCEDURE ET_RESERVATION_CHECK ( ATTR_ varchar2) IS
 -- info_           VARCHAR2(2000);
 

BEGIN

--update ifsapp.inventory_part_in_stock_tab set availability_control_id='PDI' where  contract='ETUK1' and location_no = 'QINS';
--commit; 

--Error_SYS.Record_General('CustomerOrderReservation', 'CUSRES:  Product code &NEW:PART_NO on order &NEW:ORDER_NO not able to reserve on location &NEW:LOCATION_NO due to QA only restricted.');
IFSAPP.CLIENT_SYS.Add_Info('CustomerOrderReservation', 'CUSRES:  Product code on order  not able to reserve on location due to QA only restricted.');
 
END ET_RESERVATION_CHECK;


FUNCTION ET_GET_EMAIL_BY_WO(wo_no IN VARCHAR2, Contract IN VARCHAR2) RETURN VARCHAR2 IS

   Cursor get_email(wo_no_ IN VARCHAR2, Contract_ IN VARCHAR2) is
                
        select fnd_user_property_api.Get_Value(k.reported_by_id, 'SMTP_MAIL_ADDRESS') email 
         from IFSAPP.ACTIVE_SEPARATE k 
         where k.wo_no = is_number(wo_no_)
         and  CONTRACT IN (SELECT CONTRACT FROM IFSAPP.SITE_PUBLIC  WHERE CONTRACT = IFSAPP.User_Allowed_Site_API.Authorized(Contract_));   
 
   Cursor get_email2(wo_no_ IN VARCHAR2, Contract_ IN VARCHAR2) is
        select fnd_user_property_api.Get_Value(k.signature_id, 'SMTP_MAIL_ADDRESS') email 
         from IFSAPP.MAINT_MATERIAL_REQUISITION k 
         where k.wo_no = is_number(wo_no_)
         and  CONTRACT IN (SELECT CONTRACT FROM IFSAPP.SITE_PUBLIC  WHERE CONTRACT = IFSAPP.User_Allowed_Site_API.Authorized(Contract_));   
 
 email_ varchar2(50);
 email2_ varchar2(50);
 final_email_ varchar2(100);

begin
 
     open get_email(wo_no,Contract);
          fetch get_email into email_;
     close get_email;
     
     open get_email2(wo_no,Contract);
          fetch get_email2 into email2_;
     close get_email2;
         
 final_email_ :=  nvl(email2_,email_);     
 return nvl(final_email_,'mark.calvert@elektron-technology.com');

end ET_GET_EMAIL_BY_WO;

FUNCTION ET_GET_CUS_BY_WO(wo_no IN VARCHAR2, Contract IN VARCHAR2) RETURN VARCHAR2 IS

         Cursor get_cus(wo_no_ IN VARCHAR2, Contract_ IN VARCHAR2) is
                
         select A.customer_no || ': ' || ifsapp.customer_info_api.Get_Name(A.customer_no) customer 
         from IFSAPP.ACTIVE_SEPARATE A 
         where A.wo_no = is_number(wo_no_)
         and  CONTRACT IN (SELECT CONTRACT FROM IFSAPP.SITE_PUBLIC  
         WHERE CONTRACT = IFSAPP.User_Allowed_Site_API.Authorized(Contract_)); 
         
cus_ varchar2(50);

begin
 
     open get_cus(wo_no,Contract);
          fetch get_cus into cus_;
     close get_cus;
     
 return nvl(cus_,'No customer registered');

end ET_GET_CUS_BY_WO;


FUNCTION ET_GET_EMAIL_LIST(Group_Id IN VARCHAR2) RETURN VARCHAR2 IS

   Cursor get_list(groupId_ IN VARCHAR2) is
          select wmsys.wm_concat(ifsapp.fnd_user_property_api.Get_Value(k.person_id, 'SMTP_MAIL_ADDRESS')) list_email
          from IFSAPP.DOCUMENT_GROUP_MEMBERS k where k.group_id=groupId_;
   
 emails_ varchar2(500);
 final_email_ varchar2(500);

begin
 
     open get_list(group_id);
          fetch get_list into emails_;
     close get_list;
            
 final_email_ :=  REPLACE(emails_, ',', ';');     
 return nvl(final_email_,'wasantha.kumara@elektron-technology.com....');

end ET_GET_EMAIL_LIST;

FUNCTION is_number (p_string IN VARCHAR2)
   RETURN NUMBER
IS
   v_new_num NUMBER;
BEGIN
   v_new_num := TO_NUMBER(p_string);
   RETURN v_new_num;
EXCEPTION
WHEN VALUE_ERROR THEN
   RETURN 0;
END is_number;


/*FUNCTION ET_TOOL_VALIDATE(SO_ID IN VARCHAR2, REL_NO IN VARCHAR2 , SEQ_NO IN VARCHAR2) RETURN NUMBER IS
  Cursor get_D(so_id_ IN VARCHAR2, Release_No_ IN VARCHAR2, Sequence_No_ IN VARCHAR2) is
select 1 from (select distinct 
                    --sot.contract,
                    --sot.order_no,
                    --sot.release_no,
                    --sot.sequence_no,
                    --sot.tool_id,
                    nvl(pod.phase_out, to_date('31/12/2000', 'DD/MM/YYYY')) phase_out,
                    nvl(mtd.next_calibration_date,
                        to_date('31/12/2000', 'DD/MM/YYYY')) next_cal_date
      from ifsapp.SHOP_ORDER_OPER_TOOL sot,
           (select mt.contract, mt.tool_id, max(mt.end_date) phase_out
              from MANUF_TOOL_DETAIL_AVAIL mt
             group by mt.contract, mt.tool_id) pod,
           (select mtd.contract, mtd.tool_id, mtd.next_calibration_date
              from MANUF_TOOL_DETAIL mtd) mtd
    where ORDER_NO = so_id_ and RELEASE_NO = Release_No_ and SEQUENCE_NO = Sequence_No_
       and sot.contract = pod.contract
       and sot.tool_id = pod.tool_id
       and sot.contract = mtd.contract
       and sot.tool_id = mtd.tool_id) TT
       where     
       (TT.phase_out > TT.next_cal_date 
       and TT.phase_out < sysdate)
       or 
       (TT.phase_out < sysdate 
       and  TT.next_cal_date <sysdate);
  
  Phased_out_ number := 0;
  due_calib_  number := 0;
begin

 
 FOR i_ IN get_D(SO_ID,REL_NO,SEQ_NO) LOOP
    RETURN 1;
 END LOOP;
 RETURN 0;

end ET_TOOL_VALIDATE;*/

  -----------------------------------------------------------------------------
  -------------------- FOUNDATION1 METHODS ------------------------------------
  -----------------------------------------------------------------------------
  -- Init
  --   Dummy procedure that can be called at database startup to ensure that
  --   this package is loaded into memory for performance reasons only.
  -----------------------------------------------------------------------------
  PROCEDURE Init IS
  BEGIN
    NULL;
  END Init;
END EK_CUST_EVENT_UTIL_API;
/
