CREATE OR REPLACE PACKAGE EK_CHECKIT_UTIL_API IS
module_  CONSTANT VARCHAR2(25) := 'FNDBAS';
lu_name_ CONSTANT VARCHAR2(25) := 'EkCheckitUtil';


-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
-----------------------------------------------------------------------------
PROCEDURE Handle_Errors___ (
   info_     IN OUT VARCHAR2,
   attr_     IN OUT VARCHAR2,
   err_code_ IN     NUMBER,
   err_msg_  IN     VARCHAR2 );
   
PROCEDURE Debug_Hist_Log___(
  info_  IN VARCHAR2,
  attr1_ IN VARCHAR2,
  attr2_ IN VARCHAR2,
  action_ IN VARCHAR2);

PROCEDURE Execute__ (
   info_   IN OUT VARCHAR2,
   attr_   IN OUT VARCHAR2,
   action_ IN     VARCHAR2 );
-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PROTECTED METHODS --------------------------
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
-----------------------------------------------------------------------------
PROCEDURE Trace(
   msg_ IN VARCHAR2 );

-----------------------------------------------------------------------------
-------------------- FOUNDATION1 METHODS ------------------------------------
-----------------------------------------------------------------------------

PROCEDURE Init;
END EK_CHECKIT_UTIL_API;
/
CREATE OR REPLACE PACKAGE BODY EK_CHECKIT_UTIL_API IS

rf_err_trace_ BOOLEAN;
-----------------------------------------------------------------------------
-------------------- LU SPECIFIC IMPLEMENTATION METHOD DECLARATIONS ---------
-----------------------------------------------------------------------------

PROCEDURE Handle_Trace___ (
   info_     IN OUT VARCHAR2,
   attr_     IN OUT VARCHAR2,
   action_   IN     VARCHAR2 );


-----------------------------------------------------------------------------
-------------------- LU SPECIFIC IMPLEMENTATION METHODS ---------------------
-----------------------------------------------------------------------------
PROCEDURE Debug_Hist_Log___(
  info_  IN VARCHAR2,
  attr1_ IN VARCHAR2,
  attr2_ IN VARCHAR2,
  action_ IN VARCHAR2)
IS
  PRAGMA AUTONOMOUS_TRANSACTION;
  sess_info_ VARCHAr2(200);
  xinfo_ VARCHAR2(4000);
  xattr1_ VARCHAR2(4000);
 -- xattr2_ VARCHAR2(4000) := NULL;
 -- cbr_info_ VARCHAR2(200) := null;

  Cursor get_sess_info IS
  select 'IFSUSER='|| username || ';OSUSER=' || OSUSER || '; MACHINE='|| MACHINE ||'; PROG=' ||PROGRAM
  from v$session
   where audsid=userenv('sessionid');
BEGIN
  OPEN get_sess_info;
  FETCH get_sess_info INTO sess_info_;
  CLOSE get_sess_info;

  xinfo_ := substr(replace(replace(info_,chr(31),'='),chr(30),';'),1,2000);
  xattr1_ := substr(replace(replace(attr1_,chr(31),'='),chr(30),';'),1,4000);
  
 -- cbr_info_ := Client_SYS.Get_Item_Value('#CBR#',attr1_);
  insert into EK_CHECKIT_LOG_TAB (ACTION_ID, SESSION_INFO, RESULT_STRING, DATA_STRING, DATA_STRING_LEN,ERR_STRING , ROWVERSION)
  values (action_, sess_info_, xinfo_, xattr1_,length(attr1_), attr2_,  SYSDATE);

  COMMIT;
END Debug_Hist_Log___;


PROCEDURE Handle_Errors___ (
   info_     IN OUT VARCHAR2,
   attr_     IN OUT VARCHAR2,
   err_code_ IN     NUMBER,
   err_msg_  IN     VARCHAR2 )
IS
BEGIN
   Trace_SYS.Message(err_msg_);
   IF ERROR_SYS.Is_Foundation_Error(err_code_) THEN
      attr_ := substrb(err_msg_,instrb(err_msg_,':',1,2)+2);
   ELSE
      attr_ := err_msg_;
   END IF;
   info_:= 'ERROR';
END Handle_Errors___;

PROCEDURE Handle_Trace___ (
   info_     IN OUT VARCHAR2,
   attr_     IN OUT VARCHAR2,
   action_   IN     VARCHAR2 )
IS
 --  line_state_ NUMBER;
 --  line_       VARCHAR2(300) := NULL;
BEGIN
   Client_SYS.Clear_Attr(attr_);
   rf_err_trace_ := FALSE;
   IF action_ = 'TRACE_ON' THEN
      SYS.DBMS_OUTPUT.Enable(100000);
      Trace_SYS.Set_Method_Trace(1);
      Trace_SYS.Set_Trace_Output(1);
   ELSIF action_ = 'TRACE_OFF' THEN
      SYS.DBMS_OUTPUT.Disable;
      Trace_SYS.Set_Method_Trace(0);
      Trace_SYS.Set_Trace_Output(0);
   ELSIF action_ = 'TRACE_ERR' THEN
      rf_err_trace_ := TRUE;
   END IF;
END Handle_Trace___;

-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PRIVATE METHODS ----------------------------
-----------------------------------------------------------------------------
/*
RF Test / ebug script
declare
  attr_ VARCHAR2(32000);
  info_ VARCHAR2(2000);
  action_ VARCHAr2(100);
begin
  --Change below tw parameters from log to test
  action_ := '04.VALIDATE_EXEC_COUNT';
  attr_ := 'XRF_IPX=9,192.168.0.47;LOC_NO=01HS01;PART_NO=CD600-002;SER_NO=100566;LOT_NO=*;QTY=0;TYPE=LOCATION_ONLY;';  
  
  attr_ := replace(replace(attr_,'=',chr(31)),';',chr(30));
  EK_CHECKIT_UTIL_API.Execute__(info_ ,attr_,action_);
  dbms_output.put_line(info_);
  dbms_output.put_line(attr_);
end;
*/
PROCEDURE Execute__ (
   info_   IN OUT VARCHAR2,
   attr_   IN OUT VARCHAR2,
   action_ IN     VARCHAR2 )
IS
   debug_in_attr_ VARCHAR2(4000);
   debug_out_attr_ VARCHAR2(4000);
    
  -- backtrace_ VARCHAR2(4000);
   PROCEDURE Not_Handled____ IS
   BEGIN
       info_ := 'ERROR';
       attr_ := 'Action '|| action_ || ' not handled or yet to be developed';
   END Not_Handled____;

   PROCEDURE Format_Cr_And_Display____(in_ VARCHAR2) IS
       pos_  NUMBER;
       str_  VARCHAR2(4000);
   BEGIN
       str_ := in_;
       pos_ := instrb(str_,chr(10),2);
       WHILE pos_ > 0 LOOP
           dbms_output.put_line(substrb(str_,1,pos_-1));
           str_ := substrb(str_,pos_+1);
           pos_ := instrb(str_,chr(10),2);
       END LOOP;
       dbms_output.put_line(str_);
   END Format_Cr_And_Display____;

BEGIN
   General_SYS.Init_Method(lu_name_, 'EK_CHECKIT_UTIL_API', 'Execute__');
   info_ := 'OK';
   BEGIN
      debug_in_attr_ := attr_;
      debug_out_attr_ := NULL;
      SAVEPOINT my_rf_savepoint;
      IF action_ = 'TRACE_' THEN
         Handle_Trace___(info_,attr_,action_);
      ELSIF action_ = 'CREATE_CUSTOMER_ACCOUNT' THEN
        EK_CHECKIT_UTIL2_API.Create_Customer_Account(attr_);
      ELSIF action_ = 'ADD_NEW_ADDRESS' THEN
        EK_CHECKIT_UTIL2_API.Add_New_Address(attr_);
      ELSIF action_ = 'ADD_CONTACT' THEN
        EK_CHECKIT_UTIL2_API.Add_Contact(attr_);      
      ELSIF action_ = 'ORDER_HEADER' THEN
        EK_CHECKIT_UTIL2_API.Order_Header(attr_);      
      ELSIF action_ = 'ORDER_LINE' THEN
        EK_CHECKIT_UTIL2_API.Order_Line(attr_);      
      ELSIF action_ = 'ORDER_RELEASE' THEN
        EK_CHECKIT_UTIL2_API.Order_Release(attr_);      
      ELSE
          Not_Handled____;
      END IF;
      debug_out_attr_ := attr_;
      Debug_Hist_Log___(debug_out_attr_, debug_in_attr_,NULL,action_); 

      IF info_ = 'NOTHANDLED' THEN
          Not_Handled____;
      END IF;
      IF attr_ IS NULL THEN
         -- Sending null attr_ will result vb errors!
         attr_ := 'DBRFOP_OK';
      END IF;
      COMMIT;
   EXCEPTION
      WHEN OTHERS THEN
          IF (SQLCODE = -6508) --Cannot remember what this is!
             OR (SQLCODE = -4061) THEN  --Invalidated Objects
             RAISE;
          ELSE
             ROLLBACK TO my_rf_savepoint;
             IF rf_err_trace_ THEN
                SYS.DBMS_OUTPUT.Enable(100000);
                Format_Cr_And_Display____(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
             END IF;
             Debug_Hist_Log___(debug_out_attr_, debug_in_attr_,SQLERRM,action_); 
             Handle_Errors___(info_,attr_, SQLCODE, SQLERRM);
         END IF;
   END;
END Execute__;
-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PROTECTED METHODS --------------------------
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-------------------- LU SPECIFIC PUBLIC METHODS -----------------------------
-----------------------------------------------------------------------------
PROCEDURE Trace(
   msg_ IN VARCHAR2 )
IS
BEGIN
    -- This will show the Trace in Violet color in RT Manger Debug Console!
    Trace_SYS.Message('AUTORF-> '||msg_);
END Trace;

-----------------------------------------------------------------------------
-------------------- FOUNDATION1 METHODS ------------------------------------
-----------------------------------------------------------------------------
-- Init
--   Dummy procedure that can be called at database startup to ensure that
--   this package is loaded into memory for performance reasons only.
-----------------------------------------------------------------------------

PROCEDURE Init
IS
BEGIN
   NULL;
END Init;


END EK_CHECKIT_UTIL_API;
/
