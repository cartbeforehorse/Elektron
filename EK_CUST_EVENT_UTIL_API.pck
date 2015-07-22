CREATE OR REPLACE PACKAGE EK_CUST_EVENT_UTIL_API IS

FUNCTION Et_Park_SO (
   so_id_   IN VARCHAR2,
   rel_no_  IN VARCHAR2,
   seq_no_  IN VARCHAR2 ) RETURN NUMBER;

FUNCTION Et_Tool_Validate (
   so_id_     IN VARCHAR2,
   rel_no_    IN VARCHAR2,
   seq_no_    IN VARCHAR2 ) RETURN NUMBER;

FUNCTION Et_Analysis_Issues_Exist (
   analysis_no_  IN VARCHAR2,
   contract_     IN VARCHAR2 ) RETURN NUMBER;

PROCEDURE Et_Do_Park (
   so_id_     IN VARCHAR2,
   rel_no_    IN VARCHAR2,
   seq_no_    IN VARCHAR2 );

PROCEDURE Et_Do_Unreserve (
   so_id_      IN VARCHAR2,
   rel_no_     IN VARCHAR2,
   seq_no_     IN VARCHAR2,
   line_it_no_ IN VARCHAR2);

PROCEDURE Et_Do_Park_Now (
   passattr_     IN VARCHAR2 );
    
PROCEDURE Et_Do_Unreserve_Now (
   passattr_     IN VARCHAR2 );

FUNCTION Et_II_Commission_Chk (
   inv_id_      IN NUMBER,
   company_     IN VARCHAR2 ) RETURN NUMBER;

FUNCTION Et_Tool_Label_Print (
   tool_rec_      IN CLOB,
   rec_num_       IN NUMBER ) RETURN VARCHAR2;

FUNCTION Et_Get_Lot_Batch_Size (
   order_type_    IN VARCHAR2,
   order_no_      IN VARCHAR2,
   line_no_       IN NUMBER,
   release_no_    IN VARCHAR2,
   receipt_no_    IN NUMBER,
   sequence_no_   IN VARCHAR2,
   analysis_no_   IN VARCHAR2 ) RETURN NUMBER;

FUNCTION Et_Non_Conforms (
   analysis_no_   IN VARCHAR2 ) RETURN NUMBER;

FUNCTION Et_Out_Spec (
   analysis_no_   IN VARCHAR2 ) RETURN NUMBER;

PROCEDURE Et_Do_Release_Now (
   passattr_      IN VARCHAR2 );

PROCEDURE Et_So_Release (
   so_id_         IN VARCHAR2,
   rel_no_        IN VARCHAR2,
   seq_no_        IN VARCHAR2 );

PROCEDURE Et_Tool_Updated (
   --tool_ins_id_   IN VARCHAR2,
   contract_      IN VARCHAR2 );

PROCEDURE Et_Reservation_Check;

FUNCTION Et_Get_Email_By_WO (
   wo_no_     IN VARCHAR2,
   contract_  IN VARCHAR2 ) RETURN VARCHAR2;

FUNCTION ET_GET_EMAIL_LIST(Group_Id IN VARCHAR2, Person_ID IN VARCHAR2) RETURN VARCHAR2;

FUNCTION ET_GET_EMAIL_LIST2(step_no IN NUMBER, key_ref IN VARCHAR2) RETURN VARCHAR2;

FUNCTION force_to_number (
   string_    IN VARCHAR2 ) RETURN NUMBER;
    

END EK_CUST_EVENT_UTIL_API;
/
CREATE OR REPLACE PACKAGE BODY EK_CUST_EVENT_UTIL_API IS

FUNCTION Et_Park_SO (
   so_id_   IN VARCHAR2,
   rel_no_  IN VARCHAR2,
   seq_no_  IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR get_d (so_id_c_ IN VARCHAR2, release_no_ IN VARCHAR2, sequence_no_ IN VARCHAR2) IS
      SELECT distinct sot.contract,
                      sot.order_no,
                      sot.release_no,
                      sot.sequence_no,
                      sot.tool_id,
                      nvl(pod.phase_out, to_date('31/12/2050', 'DD/MM/YYYY')) phase_out,
                      nvl(mtd.next_calibration_date, to_date('31/12/2050', 'DD/MM/YYYY')) next_cal_date
      FROM   shop_order_oper_tool sot,
             ( SELECT mt.contract, mt.tool_id, max(mt.end_date) phase_out
               FROM   manuf_tool_detail_avail mt
               GROUP BY mt.contract, mt.tool_id ) pod,
             ( SELECT mtd.contract, mtd.tool_id, mtd.next_calibration_date
               FROM   manuf_tool_detail mtd ) mtd
      WHERE  sot.order_no = so_id_c_
        AND  sot.release_no = release_no_
        AND  sot.sequence_no = sequence_no_
        AND  sot.contract = pod.contract
        AND  sot.tool_id = pod.tool_id
        AND  sot.contract = mtd.contract
        AND  sot.tool_id = mtd.tool_id;

  phased_out_   NUMBER := 0;
  due_calib_    NUMBER := 0;

BEGIN
   FOR i_ IN get_d (so_id_, rel_no_, seq_no_) LOOP
      IF (i_.phase_out < sysdate) THEN
         phased_out_ := phased_out_ + 1;
      END IF;
      IF (i_.next_cal_date <= sysdate) THEN
         due_calib_ := due_calib_ + 1;
      END IF;
   END LOOP;
   IF (phased_out_ > 0 or due_calib_ > 0) THEN
      RETURN 1;
   ELSE
      RETURN 0;
   END IF;
END Et_Park_SO;


PROCEDURE Et_Do_Park (
   so_id_     IN VARCHAR2,
   rel_no_    IN VARCHAR2,
   seq_no_    IN VARCHAR2 )
IS
   attr_  VARCHAR2(2000);
BEGIN
   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('SO_ID',  so_id_,  attr_);
   Client_SYS.Add_To_Attr ('REL_NO', rel_no_, attr_);
   Client_SYS.Add_To_Attr ('SEQ_NO', seq_no_, attr_);
   Transaction_SYS.Deferred_Call ('Ek_Cust_Event_Util_API.Et_Do_Park_Now', attr_, 'Doing Parking of SO (Elektron Custom Job)');
END Et_Do_Park;


PROCEDURE Et_Do_Unreserve (
   so_id_      IN VARCHAR2,
   rel_no_     IN VARCHAR2,
   seq_no_     IN VARCHAR2,
   line_it_no_ IN VARCHAR2)
IS
   attr_  VARCHAR2(2000);
BEGIN
   Client_SYS.Add_Info ('ShopOrder', 'You cannot issue items to a Shop Order having tools due for caliberation');
   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('SO_ID',      so_id_,      attr_);
   Client_SYS.Add_To_Attr ('REL_NO',     rel_no_,     attr_);
   Client_SYS.Add_To_Attr ('SEQ_NO',     seq_no_,     attr_);
   Client_SYS.Add_To_Attr ('LINE_IT_NO', line_it_no_, attr_);
   Transaction_SYS.Deferred_Call ('EK_CUST_EVENT_UTIL_API.ET_DO_UNRESERVE_NOW', attr_, 'Doing Unreserve of SO (Elektron Custom Job)');
END Et_Do_Unreserve;


PROCEDURE Et_Do_Park_Now (
   passattr_     IN VARCHAR2 )
IS
   info_       VARCHAR2(2000);
   attr_       VARCHAR2(2000);
   so_no_      shop_ord.order_no%TYPE;
   rel_no_     shop_ord.release_no%TYPE;
   seq_no_     shop_ord.sequence_no%TYPE;
   CURSOR get_version IS
      SELECT objid, objversion
      FROM   shop_ord so
      WHERE  so.order_no = so_no_
        AND  so.release_no = rel_no_
        AND  so.sequence_no = seq_no_;
BEGIN
   so_no_  := Client_SYS.Get_Item_Value ('SO_ID', passattr_);
   rel_no_ := Client_SYS.Get_Item_Value ('REL_NO', passattr_);
   seq_no_ := Client_SYS.Get_Item_Value ('SEQ_NO', passattr_);
   FOR i_ in get_version LOOP
      Shop_Ord_API.Park__ (info_, i_.objid, i_.objversion, attr_, 'DO');
   END LOOP;
END Et_Do_Park_Now;


PROCEDURE Et_Do_Unreserve_Now (
   passattr_ IN VARCHAR2 )
IS
   info_        VARCHAR2(2000);
   attr_        VARCHAR2(2000);
   so_no_       shop_material_alloc.order_no%TYPE;
   rel_no_      shop_material_alloc.release_no%TYPE;
   seq_no_      shop_material_alloc.sequence_no%TYPE;
   line_it_no_  shop_material_alloc.line_item_no%TYPE;
BEGIN
   so_no_      := Client_SYS.Get_Item_Value ('SO_ID', passattr_);
   rel_no_     := Client_SYS.Get_Item_Value ('REL_NO', passattr_);
   seq_no_     := Client_SYS.Get_Item_Value ('SEQ_NO', passattr_);
   line_it_no_ := Client_SYS.Get_Item_Value ('LINE_IT_NO', passattr_);
   Shop_Material_Alloc_API.Unreserve (info_, attr_, so_no_, rel_no_, seq_no_, line_it_no_);
END Et_Do_Unreserve_Now;

FUNCTION Et_II_Commission_Chk (
   inv_id_      IN NUMBER,
   company_     IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR chk_invd IS
      SELECT count(t.item_id) cnt
      FROM   inv_accounting_row_tab t
      WHERE  t.company = company_
        AND  t.invoice_id = inv_id_
        AND  (    (t.code_a = 6050 AND t.code_b = 615)
               OR (t.code_a = 6050 AND t.code_b = 610)
               OR (t.code_a = 2902 ) )
      GROUP BY t.company, t.invoice_id;
   ret_num_    NUMBER;
BEGIN
   OPEN  chk_invd;
   FETCH chk_invd INTO ret_num_;
   CLOSE chk_invd;
   RETURN nvl (ret_num_, 0);
END Et_II_Commission_Chk;


FUNCTION ET_Tool_Label_print (
   tool_rec_  CLOB,
   rec_num_   NUMBER ) RETURN VARCHAR2
IS
   tool_list_ VARCHAR2(2000);
   ret_str_ VARCHAR2(2000);
   stmt_      VARCHAR2(2000);
   stmt1_     VARCHAR2(2000);
   stmt2_     VARCHAR2(2000);
   tool_      varchar2(100);
   i_          number;
   out_        number;
   rec_length_ number;
   init_len_   number;
BEGIN
   i_ := 0;
   init_len_   := 1;
   DELETE ek_tool_print_tab;
   DELETE ek_tool_id_print_tab;
   COMMIT;
   INSERT INTO ek_tool_print_tab VALUES (tool_rec_);
   COMMIT;

   tool_list_ := chr(39);
   out_       := rec_num_;
   WHILE (out_ != 0) LOOP
      i_ := i_ + 1;
      stmt1_ := 'BEGIN select instr(DBMS_LOB.substr(t.tool_str,4000,:init_len_),' || '''' ||
                '-$0:' || '''' ||
                ',1,:i_) INTO :rec_length_ from ifsapp.EK_TOOL_PRINT_TAB t; END;';
      EXECUTE IMMEDIATE stmt1_
      USING
         IN init_len_,
         IN i_,
         OUT rec_length_;

      IF rec_length_ = 0 THEN
         Trace_SYS.Message ('Inside IF......... ' || to_char(i_));
         init_len_ := init_len_ + 4000;
         i_ := 1;
      END IF;
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
               ')+1) INTO :tool_ FROM ek_tool_print_tab t; END;';
      EXECUTE IMMEDIATE stmt_
      USING
         IN init_len_,
         IN i_,
         OUT tool_;
      Trace_SYS.Message ('I less than ' || to_char(i_) || ' tool_ -------------->' || tool_ || '    ' || rec_length_);
      out_ := out_ - 1;
      INSERT INTO ek_tool_id_print_tab VALUES (tool_);
      COMMIT;
      IF out_ > 0 THEN
         tool_list_ := tool_list_ || tool_ || chr(39) || chr(44) || chr(39);
      ELSE
         tool_list_ := tool_list_ || tool_ || chr(39);
      END IF;
   END LOOP;
   Trace_SYS.Message ('tool_list_ -->' || tool_list_);
   stmt2_ :=
      'BEGIN
          SELECT collect_func_long (cast (collect (s.tool_id) as ifsapp.mystringtabletypelong),'||''''||''''||''''||','||''''||''''||''''||')
          INTO   :ret_str_
          FROM   ( select distinct tool_id from ek_tool_id_print_tab t ) s;
       END;';
   EXECUTE IMMEDIATE stmt2_ USING IN OUT ret_str_;
   Trace_SYS.Message ('ret_str_ -->  ' || chr(39) || ret_str_ || chr(39));
   RETURN chr(39) || ret_str_ || chr(39);

END Et_Tool_Label_print;


FUNCTION Et_Get_Lot_Batch_Size (
   order_type_    IN VARCHAR2,
   order_no_      IN VARCHAR2,
   line_no_       IN NUMBER,
   release_no_    IN VARCHAR2,
   receipt_no_    IN NUMBER,
   sequence_no_   IN VARCHAR2,
   analysis_no_   IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR get_purch_lot_size IS
      SELECT t1.qty_arrived
      FROM   Purchase_Receipt t1
      WHERE  t1.order_no = order_no_
        AND  t1.line_no = line_no_
        AND  t1.release_no = release_no_
        AND  t1.receipt_no = receipt_no_;

   CURSOR get_shop_lot_size IS
      SELECT t2.revised_qty_due
      FROM   shop_ord t2
      WHERE  t2.order_no = order_no_
        AND  t2.release_no = release_no_
        AND  t2.sequence_no = sequence_no_;

   CURSOR get_sample_qty IS
      SELECT max (k.qty_to_control)
      FROM   qman_norm_for_control_plan k 
      WHERE  k.analysis_no = analysis_no_;
                                
   lot_size_  NUMBER;

BEGIN
   IF order_type_ = 'PurchOrderAnalysis' THEN
      OPEN  get_purch_lot_size;
      FETCH get_purch_lot_size INTO lot_size_;
      CLOSE get_purch_lot_size;
   ELSIF order_type_ = 'InventoryAnalysis' THEN
      OPEN  get_sample_qty;
      FETCH get_sample_qty INTO lot_size_;
      CLOSE get_sample_qty;
   ELSE
      OPEN  get_shop_lot_size;
      FETCH get_shop_lot_size INTO lot_size_;
      CLOSE get_shop_lot_size;
   END IF;
   RETURN nvl (lot_size_, 0);
END Et_Get_Lot_Batch_Size;


FUNCTION Et_Non_Conforms (
   analysis_no_   IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR get_non_conforms IS
      SELECT sum(t.non_conform_qty)
      FROM   qman_norm_for_control_plan t 
      WHERE  t.analysis_no = analysis_no_
      GROUP BY t.analysis_no;
   ret_num_  NUMBER;
BEGIN
   OPEN  get_non_conforms;
   FETCH get_non_conforms INTO ret_num_;
   CLOSE get_non_conforms;
   RETURN nvl (ret_num_, 0);
END Et_Non_Conforms;

--no of out of spec 
FUNCTION Et_Out_Spec (
   analysis_no_   IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR get_out_spec IS
      SELECT count(1)
      FROM   qman_sample_value h
      WHERE  h.analysis_no = analysis_no_
        AND  h.result_status_db = 'OUT OF SPECIFICATION';
   ret_num_  NUMBER;
BEGIN
   OPEN  get_out_spec;
   FETCH get_out_spec INTO ret_num_;
   CLOSE get_out_spec;
   RETURN nvl (ret_num_, 0);
END Et_Out_Spec;


FUNCTION Et_Analysis_Issues_Exist (
   analysis_no_  IN VARCHAR2,
   contract_     IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR get_analysis IS
      SELECT 1
      FROM   analysis_full af
      WHERE  af.control_plan_no IS NOT NULL
        AND  af.ctrl_plan_revision_no IS NOT NULL 
        AND  af.analysis_no = analysis_no_
        AND  af.contract = contract_
        AND  User_Allowed_Site_API.Is_Authorized(af.contract) = 1
        AND  (    af.attribute_result_status = 'Non Conformities Exists'
               OR af.variable_result_status IN ('Out of Specification', 'Partly In Specification')
             )
        AND  af.state = 'Confirmed';
BEGIN
   FOR i_ IN get_analysis LOOP
      RETURN 1;
   END LOOP;
   RETURN 0;
END Et_Analysis_Issues_Exist;


FUNCTION Et_Tool_Validate (
   so_id_     IN VARCHAR2,
   rel_no_    IN VARCHAR2,
   seq_no_    IN VARCHAR2 ) RETURN NUMBER
IS
   CURSOR get_d (so_id_c_ IN VARCHAR2, release_no_ IN VARCHAR2, sequence_no_ IN VARCHAR2) IS
      SELECT 1 FROM ( SELECT distinct s.revised_start_date start_date, s.revised_due_date finish_date,
                                      nvl(mtd.next_calibration_date, to_date('31/12/2050', 'DD/MM/YYYY')) next_cal_date
                      FROM   shop_order_oper_tool sot, shop_ord s,
                             ( SELECT mtd.contract, mtd.tool_id, mtd.next_calibration_date
                               FROM   manuf_tool_detail mtd ) mtd
                      WHERE  s.order_no = so_id_c_
                        AND  s.release_no = release_no_
                        AND  s.sequence_no = sequence_no_
                        AND  sot.contract = mtd.contract
                        AND  sot.tool_id = mtd.tool_id
                        AND  sot.order_no = s.order_no
                        AND  sot.sequence_no = s.sequence_no
                        AND  sot.release_no = s.release_no
                        AND  sot.contract = s.contract
                     ) tt
      WHERE  tt.next_cal_date BETWEEN start_date AND finish_date
         OR  tt. next_cal_date < start_date;

BEGIN

   FOR i_ IN get_d (so_id_, rel_no_, seq_no_) LOOP
      RETURN 1;
   END LOOP;
   RETURN 0;
END Et_Tool_Validate;


PROCEDURE Et_Do_Release_Now (
   passattr_      IN VARCHAR2 )
IS
   info_         VARCHAR2(2000);
   attr_         VARCHAR2(2000);
   so_no_        shop_ord.order_no%TYPE;
   rel_no_       shop_ord.release_no%TYPE;
   seq_no_       shop_ord.sequence_no%TYPE;

   CURSOR get_version IS
      SELECT /*ROWID, ltrim(lpad(to_char(rowversion,'YYYYMMDDHH24MISS'),2000))*/ objid, objversion
      FROM  SHOP_ORD
      WHERE  order_no = so_no_
      AND   release_no = rel_no_
      AND   sequence_no = seq_no_;
BEGIN
   so_no_  := Client_SYS.Get_Item_Value ('SO_ID', passattr_);
   rel_no_ := Client_SYS.Get_Item_Value ('REL_NO', passattr_);
   seq_no_ := Client_SYS.Get_Item_Value ('SEQ_NO', passattr_);
   FOR i_ in get_version LOOP
      trace_sys.Message('OBJID ----->'||i_.objid);
      Shop_Ord_API.Release__ (info_, i_.objid, i_.objversion, attr_, 'DO');
   END LOOP;
END Et_Do_Release_Now;


PROCEDURE Et_So_Release (
   so_id_         IN VARCHAR2,
   rel_no_        IN VARCHAR2,
   seq_no_        IN VARCHAR2 )
IS
   attr_  VARCHAR2(2000);
BEGIN
   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('SO_ID',  so_id_,  attr_);
   Client_SYS.Add_To_Attr ('REL_NO', rel_no_, attr_);
   Client_SYS.Add_To_Attr ('SEQ_NO', seq_no_, attr_);
   Transaction_SYS.Deferred_Call('Ek_Cust_Event_Util_API.Et_Do_Release_Now', attr_, 'Release SO:'|| so_id_);
END ET_SO_Release;

-- this is not using but for the test
PROCEDURE Et_Tool_Updated (
   --tool_ins_id_   IN VARCHAR2,
   contract_      IN VARCHAR2 )
IS
   CURSOR get_parked_so IS
      SELECT  order_no, release_no, sequence_no
      FROM    shop_ord so
      WHERE   so.contract = contract_
        AND   so.order_no = '46863' 
        AND   so.state = 'Parked';
BEGIN
   FOR i_ in get_parked_so LOOP
      Et_SO_Release (i_.order_no, i_.release_no, i_.sequence_no);
   END LOOP;
END Et_Tool_Updated;


PROCEDURE Et_Reservation_Check
IS BEGIN
   Client_SYS.Add_Info ('CustomerOrderReservation', 'CUSRES: Product code on order not able to reserve on location due to QA only restricted.');
END Et_Reservation_Check;


FUNCTION Et_Get_Email_By_WO (
   wo_no_     IN VARCHAR2,
   contract_  IN VARCHAR2 ) RETURN VARCHAR2
IS
   CURSOR get_email IS
      SELECT Fnd_User_Property_API.Get_Value (k.reported_by_id, 'SMTP_MAIL_ADDRESS') email
      FROM   active_separate k
      WHERE  k.wo_no = force_to_number(wo_no_)
        AND  k.contract = contract_
        AND  User_Allowed_Site_API.Is_Authorized(k.contract) = 1;

   CURSOR get_email2 IS
      SELECT Fnd_User_Property_API.Get_Value (k.signature_id, 'SMTP_MAIL_ADDRESS') email
      FROM   maint_material_requisition k
      WHERE  k.wo_no = force_to_number(wo_no_) 
        AND  k.contract = contract_
        AND  User_Allowed_Site_API.Is_Authorized(k.contract) = 1;

   email_        fnd_user_property_tab.value%TYPE;
   email2_       fnd_user_property_tab.value%TYPE;
   final_email_  fnd_user_property_tab.value%TYPE;

BEGIN
   OPEN  get_email;
   FETCH get_email INTO email_;
   CLOSE get_email;

   OPEN  get_email2;
   FETCH get_email2 INTO email2_;
   CLOSE get_email2;

   final_email_ :=  nvl (email2_, email_);
   RETURN nvl (final_email_, 'mark.calvert@elektron-technology.com');

END Et_Get_Email_By_WO;

FUNCTION Et_Get_Cus_By_WO (
   wo_no_     IN VARCHAR2,
   contract_  IN VARCHAR2 ) RETURN VARCHAR2
IS
   CURSOR get_cus IS
      SELECT a.customer_no || ': ' || Customer_Info_API.Get_Name(a.customer_no) customer_address_leadtime
      FROM   active_separate a
      WHERE  a.wo_no = force_to_number(wo_no_)
        AND  a.contract = contract_
        AND  User_Allowed_Site_API.Is_Authorized(a.contract) = 1;
   cus_    VARCHAR2(50);
BEGIN
   OPEN  get_cus;
   FETCH get_cus INTO cus_;
   CLOSE get_cus;
   RETURN nvl (cus_, 'No customer registered');
END Et_Get_Cus_By_WO;


FUNCTION ET_GET_EMAIL_LIST(Group_Id IN VARCHAR2, Person_ID IN VARCHAR2) RETURN VARCHAR2 IS

   Cursor get_list(groupId_ IN VARCHAR2, person_id_ IN VARCHAR2) is
          select wmsys.wm_concat(ifsapp.fnd_user_property_api.Get_Value(k.person_id, 'SMTP_MAIL_ADDRESS')) list_email
          from IFSAPP.DOCUMENT_GROUP_MEMBERS k 
          where k.group_id=groupId_ 
          or k.person_id=person_id_;
          
 emails_ varchar2(500);
 final_email_ varchar2(500);

begin
 
     open get_list(group_id, person_id);
          fetch get_list into emails_;
     close get_list;
            
 final_email_ :=  REPLACE(emails_, ',', ';');     
 return nvl(final_email_,'wasantha.kumara@elektron-technology.com');

end ET_GET_EMAIL_LIST;

FUNCTION ET_GET_EMAIL_LIST2(step_no IN NUMBER, key_ref IN VARCHAR2) RETURN VARCHAR2 IS

   Cursor get_ids(step_no_ IN NUMBER, key_ref_ IN VARCHAR2) is
          select a.person_id, a.group_id
          from IFSAPP.APPROVAL_ROUTING a
          where a.key_ref = key_ref_
          and a.step_no = step_no_; 
  /*        
   Cursor get_list(groupId_ IN VARCHAR2, person_id_ IN VARCHAR2) is
          select wmsys.wm_concat(ifsapp.fnd_user_property_api.Get_Value(k.person_id, 'SMTP_MAIL_ADDRESS')) list_email
          from IFSAPP.DOCUMENT_GROUP_MEMBERS k 
          where k.group_id=groupId_ 
          or k.person_id=person_id_; */         
        
   
 --emails_ varchar2(500);
 --final_email_ varchar2(500);
 group_id_ varchar2(20);
 person_id_ varchar2(20);

begin
 
--trace_sys.Message('step no  ********************************************************:'||step_no);
--trace_sys.Message('key_ref  *******************************************************************:'||key_ref);
Dbms_Output.put_line('step no  *******************************************************************:'||step_no);
Dbms_Output.put_line('key_ref  *******************************************************************:'||key_ref);


     open get_ids(step_no, key_ref);
          fetch get_ids into person_id_, group_id_;
     close get_ids;

            --group_id_ := null;
            --person_id_ :='WAKUGB';
            
     return ET_GET_EMAIL_LIST(group_id_, person_id_);

   /*  open get_list(group_id_, person_id_);
          fetch get_list into emails_;
     close get_list;
            
 final_email_ :=  REPLACE(emails_, ',', ';');     
 return nvl(final_email_,'wasantha.kumara@elektron-technology.com');*/
 

end ET_GET_EMAIL_LIST2;




FUNCTION force_to_number (
   string_    IN VARCHAR2 ) RETURN NUMBER
IS
   new_num_  NUMBER;
BEGIN
   new_num_ := to_number (string_);
   RETURN new_num_;
EXCEPTION
   WHEN VALUE_ERROR THEN
      RETURN 0;
END force_to_number;


END EK_CUST_EVENT_UTIL_API;
/
