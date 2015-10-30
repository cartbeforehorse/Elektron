CREATE OR REPLACE PACKAGE EK_CHECKIT_API IS

   module_  CONSTANT VARCHAR2(25) := 'FNDBAS';
   lu_name_ CONSTANT VARCHAR2(25) := 'EkCheckit';
  
   PROCEDURE Create_Customer_With_Addresses (
      customer_id_           OUT VARCHAR2,
      cust_name_             IN  VARCHAR2,
      language_              IN  VARCHAR2,
      cust_country_code_     IN  VARCHAR2,
      cust_reference_        IN  VARCHAR2,
      sales_coodinator_      IN  VARCHAR2, 
      address_country_code_  IN  VARCHAR2,
      addr_name_             IN  VARCHAR2,
      addr1_                 IN  VARCHAR2,
      addr2_                 IN  VARCHAR2,
      city_                  IN  VARCHAR2,
      county_                IN  VARCHAR2,
      state_                 IN  VARCHAR2,
      zip_                   IN  VARCHAR2,
      address_country_code2_ IN  VARCHAR2,
      addr_name2_            IN  VARCHAR2,
      addr21_                IN  VARCHAR2,
      addr22_                IN  VARCHAR2,
      city2_                 IN  VARCHAR2,
      county2_               IN  VARCHAR2,
      state2_                IN  VARCHAR2,
      zip2_                  IN  VARCHAR2 );
  
   PROCEDURE Create_Customer_Account (
      cust_name_        IN  VARCHAR2,
      language_         IN  VARCHAR2,
      country_code_     IN  VARCHAR2,
      cust_reference_   IN  VARCHAR2,     
      sales_coodinator_ IN  VARCHAR2,        
      customer_id_      OUT VARCHAR2 );
  
   PROCEDURE Add_Contact (
      customer_id_ IN VARCHAR2,
      com_method_  IN VARCHAR2,
      name_        IN VARCHAR2,
      description_ IN VARCHAR2,
      value_       IN VARCHAR2,
      method_def_  IN VARCHAR2,
      address_id_  IN VARCHAR2 default null );
      
   PROCEDURE Add_Address___ (
      addr_id_            IN VARCHAR2, 
      cust_id_            IN VARCHAR2,
      addr_country_code_  IN VARCHAR2,
      addr_name_          IN VARCHAR2,
      addr1_              IN VARCHAR2,
      addr2_              IN VARCHAR2,
      city_               IN VARCHAR2,
      county_             IN VARCHAR2,
      state_              IN VARCHAR2,
      zip_                IN VARCHAR2,
      address_type_       IN VARCHAR2 default null );
  
   PROCEDURE Add_New_Address (
      customer_id_          IN VARCHAR2,
      address_country_code_ IN VARCHAR2,
      addr_name_            IN VARCHAR2,
      addr1_                IN VARCHAR2,
      addr2_                IN VARCHAR2,
      city_                 IN VARCHAR2,
      county_               IN VARCHAR2,
      state_                IN VARCHAR2,
      zip_                  IN VARCHAR2,
      address_type_         IN VARCHAR2 DEFAULT NULL,
      address_id            OUT VARCHAR2 );
 

   PROCEDURE Add_Payinfo__ (
      customer_id_ IN VARCHAR2 );
            
   PROCEDURE Customer_Exist (
      cust_name_            IN  VARCHAR2,
      language_             IN  VARCHAR2 DEFAULT NULL,
      cust_country_code_    IN  VARCHAR2 DEFAULT NULL,
      cust_reference_       IN  VARCHAR2 DEFAULT NULL,
      addr_id_              IN  VARCHAR2 DEFAULT NULL, 
      address_country_code_ IN  VARCHAR2 DEFAULT NULL,
      addr_name_            IN  VARCHAR2 DEFAULT NULL,
      addr1_                IN  VARCHAR2 DEFAULT NULL,
      addr2_                IN  VARCHAR2 DEFAULT NULL,
      city_                 IN  VARCHAR2 DEFAULT NULL,
      county_               IN  VARCHAR2 DEFAULT NULL,
      state_                IN  VARCHAR2 DEFAULT NULL,
      zip_                  IN  VARCHAR2 DEFAULT NULL,
      is_exist_             OUT VARCHAR2 );
  
   PROCEDURE Order_Header(
      customer_no_        IN  VARCHAR2,
      checkit_order_type_ IN  VARCHAR2,
      coordinator_        IN  VARCHAR2,
      parent_order_       IN  VARCHAR2,
      order_no_           OUT VARCHAR2 );

   PROCEDURE Order_Line__ (
      order_no_  IN VARCHAR2,
      part_no_   IN VARCHAR2,
      quantity_  IN NUMBER,
      price_     IN NUMBER,
      discount_  IN NUMBER );

   PROCEDURE New_Order_Line (
      order_no_   IN VARCHAR2,
      part_no_    IN VARCHAR2,
      quantity_   IN NUMBER,
      price_      IN NUMBER,
      discount_   IN NUMBER );

   PROCEDURE New_Order_With_Line (
      order_no_      OUT  VARCHAR2,
      customer_no_   IN  VARCHAR2,
      order_type_    IN  VARCHAR2,
      coordinator_   IN  VARCHAR2, 
      parent_order_  IN  VARCHAR2,   
      part_no_       IN  VARCHAR2,
      quantity_      IN  NUMBER,
      price_         IN  NUMBER,
      discount_      IN  NUMBER );

   PROCEDURE Order_Release(
      order_no_     IN VARCHAR2,
      woo_order_no_ IN VARCHAR2 );
  
   PROCEDURE Add_Technical_Chars (
      order_no_        IN VARCHAR2,
      technical_class_ IN VARCHAR2 );
    
   PROCEDURE Update_Technical_Chars (
      order_no_        IN VARCHAR2,
      technical_class_ IN VARCHAR2,
      attribute_       IN VARCHAR2,
      woo_order_no_    IN VARCHAR2 );
  
   PROCEDURE Update_Spec__ (
      spec_no_         IN NUMBER,
      attribute_       IN VARCHAR2,
      attribute_value_ IN VARCHAR2 );
      
   PROCEDURE Create_Relationship (
      contract_    IN VARCHAR2,
      customer_no_ IN VARCHAR2,
      order_no_    IN VARCHAR2 );
    
   PROCEDURE Init;
END EK_CHECKIT_API;
/
CREATE OR REPLACE PACKAGE BODY EK_CHECKIT_API IS

-----------------------------------------------------------------------------
-------------------- GLOBAL LU CONSTANTS ------------------------------------
-----------------------------------------------------------------------------
inst_PurchaseOrderLine_  CONSTANT BOOLEAN      := Dictionary_SYS.Logical_Unit_Is_Installed('PurchaseOrderLine');
checkit_site_            CONSTANT VARCHAR2(20) := USER_ALLOWED_SITE_API.Get_default_site;
checkit_company_         CONSTANT VARCHAR2(20) := Site_API.Get_Company(checkit_site_);
default_credit_limit_    CONSTANT number       := 10000;

/*
Date: 25/04/2015
Author: Wasantha Kumara
Purpose: This is custom API for elektron checkit e-commerce integration covering customer and order flow.
*/

PROCEDURE Create_Customer_With_Addresses (
   customer_id_           OUT VARCHAR2,
   cust_name_             IN  VARCHAR2,
   language_              IN  VARCHAR2,
   cust_country_code_     IN  VARCHAR2,
   cust_reference_        IN  VARCHAR2,
   sales_coodinator_      IN  VARCHAR2, 
   address_country_code_  IN  VARCHAR2,
   addr_name_             IN  VARCHAR2,
   addr1_                 IN  VARCHAR2,
   addr2_                 IN  VARCHAR2,
   city_                  IN  VARCHAR2,
   county_                IN  VARCHAR2,
   state_                 IN  VARCHAR2,
   zip_                   IN  VARCHAR2,
   address_country_code2_ IN  VARCHAR2,
   addr_name2_            IN  VARCHAR2,
   addr21_                IN  VARCHAR2,
   addr22_                IN  VARCHAR2,
   city2_                 IN  VARCHAR2,
   county2_               IN  VARCHAR2,
   state2_                IN  VARCHAR2,
   zip2_                  IN  VARCHAR2 )
IS BEGIN
   Create_Customer_Account (cust_name_, language_, cust_country_code_, cust_reference_, sales_coodinator_, customer_id_);
   Add_Address___ ('01', customer_id_, address_country_code_, addr_name_, addr1_, addr2_, city_, county_, state_, zip_, 'DELIVERY');
   Add_Address___ ('02', customer_id_, address_country_code2_, addr_name2_, addr21_, addr22_, city2_, county2_, state2_, zip2_, 'INVOICE');
END Create_Customer_With_Addresses;

PROCEDURE Create_Customer_Account (
   cust_name_        IN  VARCHAR2,
   language_         IN  VARCHAR2,
   country_code_     IN  VARCHAR2,
   cust_reference_   IN  VARCHAR2,     
   sales_coodinator_ IN  VARCHAR2,        
   customer_id_      OUT VARCHAR2 )
IS
   info_        VARCHAR2(2000);
   attr_        VARCHAR2(2000);
   objid_       VARCHAR2(2000);
   objver_      VARCHAR2(2000);

   ext_cust_id_ NUMBER;
   cust_id_     VARCHAR2(20);
   hattr1_      VARCHAR2(2000);
   hinfo1_      VARCHAR2(2000);
   hinfo2_      VARCHAR2(2000);

BEGIN
   General_SYS.Init_Method(lu_name_, 'EK_CHECKIT_API', 'Create_Customer_Account');
          
   SELECT ek_ext_customer_no.NEXTVAL INTO ext_cust_id_ FROM dual;
   cust_id_ := 'CKT' || ext_cust_id_;
   customer_id_ := cust_id_;

   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('CUSTOMER_ID',          cust_id_,        attr_);
   Client_SYS.Add_To_Attr ('NAME',                 cust_name_,      attr_);
   Client_SYS.Add_To_Attr ('DEFAULT_LANGUAGE',     language_,       attr_);
   Client_SYS.Add_To_Attr ('COUNTRY',              Iso_Country_API.Decode(country_code_), attr_);
   Client_SYS.Add_To_Attr ('CREATION_DATE',        SYSDATE,         attr_);
   Client_SYS.Add_To_Attr ('DEFAULT_DOMAIN',       'TRUE',          attr_);
   Client_SYS.Add_To_Attr ('PARTY_TYPE',           'Customer',      attr_);
   Client_SYS.Add_To_Attr ('IDENTIFIER_REFERENCE', cust_reference_, attr_);
   Customer_Info_API.New__ (info_, objid_, objver_, attr_, 'DO');

   Client_SYS.Clear_Attr  (attr_);
   Client_SYS.Add_To_Attr ('CUSTOMER_ID',                   cust_id_,              attr_);
   Client_SYS.Add_To_Attr ('CUST_GRP',                      'EXPORT',              attr_);
   Client_SYS.Add_To_Attr ('MARKET_CODE',                   'TOO',                 attr_);
   Client_SYS.Add_To_Attr ('CURRENCY_CODE',                 'GBP',                 attr_);
   Client_SYS.Add_To_Attr ('CONFIGURATOR_CUSTOMER_DB',      'NO_EXPORT',           attr_);
   Client_SYS.Add_To_Attr ('ORDER_ID',                      'CKT',                 attr_);
   Client_SYS.Add_To_Attr ('CYCLE_PERIOD',                  0,                     attr_);
   Client_SYS.Add_To_Attr ('INVOICE_SORT',                  Customer_Invoice_Type_API.Decode('N'), attr_);
   Client_SYS.Add_To_Attr ('CR_STOP_DB',                    'N',                   attr_);
   Client_SYS.Add_To_Attr ('ORDER_CONF_FLAG_DB',            'Y',                   attr_);
   Client_SYS.Add_To_Attr ('PACK_LIST_FLAG_DB',             'Y',                   attr_);
   Client_SYS.Add_To_Attr ('CATEGORY_DB',                   'E',                   attr_);
   Client_SYS.Add_To_Attr ('EDI_AUTO_ORDER_APPROVAL_DB',    'NO',                  attr_);
   Client_SYS.Add_To_Attr ('QUICK_REGISTERED_CUSTOMER_DB',  'NORMAL',              attr_);
   Client_SYS.Add_To_Attr ('TEMPLATE_CUSTOMER_DB',          'NOT_TEMPLATE',        attr_);
   Client_SYS.Add_To_Attr ('COMMISSION_RECEIVER_DB',        'DONOTCREATE',         attr_);
   Client_SYS.Add_To_Attr ('CONFIGURATOR_CUSTOMER_DB',      'NO_EXPORT',           attr_);
   Client_SYS.Add_To_Attr ('CUST_PART_ACQ_VAL_LEVEL',       Cust_Part_Acq_Val_Level_API.Decode('NO_ACQ'), attr_);
   Client_SYS.Add_To_Attr ('CUST_PART_OWNER_TRANSFER_DB',   'DONT_ALLOW_TRANSFER', attr_);
   Client_SYS.Add_To_Attr ('SEND_CHANGE_MESSAGE_DB',        'N',                   attr_);
   Client_SYS.Add_To_Attr ('SUMMARIZED_SOURCE_LINES_DB',    'Y',                   attr_);
   Client_SYS.Add_To_Attr ('MATCH_TYPE',                    Match_Type_API.Decode('NOAUTO'), attr_);
   Client_SYS.Add_To_Attr ('PRINT_AMOUNTS_INCL_TAX_DB',     'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('CONFIRM_DELIVERIES_DB',         'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('CHECK_SALES_GRP_DELIV_CONF_DB', 'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('UPDATE_PRICE_FROM_SBI_DB',      'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('SELF_BILLING_MATCH_OPTION',     Matching_Option_API.Decode('DELIVERY NOTE'),    attr_);
   Client_SYS.Add_To_Attr ('RECEIVING_ADVICE_TYPE',         Receiving_Advice_Type_API.Decode('DO_NOT_USE'), attr_);
   Client_SYS.Add_To_Attr ('REC_ADV_MATCHING_OPTION',       Matching_Option_API.Decode('DELIVERY NOTE'),    attr_);
   Client_SYS.Add_To_Attr ('REC_ADV_AUTO_MATCHING_DB',      'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('REC_ADV_AUTO_MATCH_DIFF_DB',    'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('ADV_INV_FULL_PAY_DB',           'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('AUTOMATIC_ORDER_RELEASE_DB',    'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('CREDIT_CONTROL_GROUP_ID',       Credit_Control_Group_API.Get_Default_Group(),   attr_);
   Client_SYS.Add_To_Attr ('BACKORDER_OPTION',              Customer_Backorder_Option_API.Decode('ALLOW PARTIAL LINE DELIVERY'), attr_);
   Client_SYS.Add_To_Attr ('SALESMAN_CODE',                 sales_coodinator_,     attr_);
   Client_SYS.Add_To_Attr ('REPLICATE_DOC_TEXT_DB',         'N',                   attr_);
   Cust_Ord_Customer_API.New__ (info_, objid_, objver_, attr_, 'DO');

   Client_SYS.Clear_Attr  (attr_);    
   Client_SYS.Add_To_Attr ('COMPANY',       checkit_company_,      attr_);
   Client_SYS.Add_To_Attr ('IDENTITY',      cust_id_,              attr_);
   Client_SYS.Add_To_Attr ('CREDIT_LIMIT',  default_credit_limit_, attr_);
   Client_SYS.Add_To_Attr ('CREDIT_BLOCK',  'FALSE',               attr_);
   Client_SYS.Add_To_Attr ('PARTY_TYPE_DB', 'CUSTOMER',            attr_);
   Customer_Credit_Info_API.New__ (info_, objid_, objver_, attr_, 'DO');


   -- Log the history of the API Call.
   Client_SYS.Clear_Attr  (hattr1_);
   Client_SYS.Add_To_Attr ('NAME',             cust_name_,      hattr1_);
   Client_SYS.Add_To_Attr ('LANGUAGE',         language_,       hattr1_);
   Client_SYS.Add_To_Attr ('COUNTRY_CODE',     country_code_,   hattr1_);
   Client_SYS.Add_To_Attr ('CUST_REFERENCE',   cust_reference_, hattr1_);
   Client_SYS.Add_To_Attr ('SALES_COODINATOR', cust_reference_, hattr1_);

   Client_SYS.Clear_Attr  (hinfo1_);
   Client_SYS.Add_To_Attr ('CUSTOMER_ID', cust_id_, hinfo1_);

   Ek_Checkit_Util_API.Debug_Hist_Log___ (hinfo1_, hattr1_, null, 'Create_Customer_Account');

EXCEPTION
   WHEN OTHERS THEN
      Ek_Checkit_Util_API.Handle_Errors___ (hinfo2_, hinfo1_, SQLCODE, SQLERRM);
      EK_CHECKIT_UTIL_API.Debug_Hist_Log___ (hinfo1_, hattr1_, null, 'Create_customer_account');
      RAISE;        
END Create_Customer_Account;

PROCEDURE Add_Address___ (
   addr_id_            IN VARCHAR2, 
   cust_id_            IN VARCHAR2,
   addr_country_code_  IN VARCHAR2,
   addr_name_          IN VARCHAR2,
   addr1_              IN VARCHAR2,
   addr2_              IN VARCHAR2,
   city_               IN VARCHAR2,
   county_             IN VARCHAR2,
   state_              IN VARCHAR2,
   zip_                IN VARCHAR2,
   address_type_       IN VARCHAR2 default null )
IS
   adinfo_    VARCHAR2(2000);
   adattr_    VARCHAR2(32000);
   curr_addr_ VARCHAR2(2000);
   in_addr_   VARCHAr2(2000);

   info_      VARCHAR2(2000);
   objid_     VARCHAR2(2000);
   objver_    VARCHAR2(2000);

   CURSOR client_addr_types IS
      SELECT 'Delivery' addr_type FROM dual
      UNION ALL
      SELECT 'Document' addr_type FROM dual
      UNION ALL
      SELECT 'Visit' addr_type FROM dual
      UNION ALL
      SELECT 'Pay' addr_type FROM dual
      UNION ALL
      SELECT 'PrimaryContact' addr_type FROM dual
      UNION ALL
      SELECT 'SecondaryContact' addr_type FROM dual;
BEGIN
   curr_addr_ := Customer_Info_Address_API.Get_Address (cust_id_, addr_id_);
   IF curr_addr_ IS NOT null THEN
      in_addr_ := Address_Presentation_API.Format_Address (
                     addr_country_code_, addr1_, addr2_, city_, county_,
                     state_, zip_, Iso_Country_API.Decode(addr_country_code_));
      IF curr_addr_ != in_addr_ THEN
         Client_SYS.Clear_Attr  (adattr_);
         Client_SYS.Add_To_Attr ('COUNTRY',  Iso_Country_API.Decode (addr_country_code_), adattr_);
         Client_SYS.Add_To_Attr ('ADDRESS1', addr1_,  adattr_);
         Client_SYS.Add_To_Attr ('ADDRESS2', addr2_,  adattr_);
         Client_SYS.Add_To_Attr ('ZIP_CODE', zip_,    adattr_);
         Client_SYS.Add_To_Attr ('COUNTY',   county_, adattr_);
         Client_SYS.Add_To_Attr ('STATE',    state_,  adattr_);
         Client_SYS.Add_To_Attr ('CITY',     city_,   adattr_);
         Customer_Info_Address_API.Modify_Address (cust_id_, addr_id_, adattr_);
      END IF;
      Client_SYS.Add_To_Attr ('COMPANY_NAME2',   addr_name_, adattr_);
      Client_SYS.Add_To_Attr ('SHIP_VIA_CODE',  '003',       adattr_);
      Client_SYS.Add_To_Attr ('DELIVERY_TERMS', 'EXW',       adattr_);
      Cust_Ord_Customer_Address_API.Modify (adinfo_, adattr_, cust_id_, addr_id_);
      RETURN;
   END IF;
   Client_SYS.Clear_Attr  (adattr_);
   Client_SYS.Add_To_Attr ('ADDRESS_ID',  addr_id_,  adattr_);
   Client_SYS.Add_To_Attr ('COUNTRY',     Iso_Country_API.Decode(addr_country_code_), adattr_);
   Client_SYS.Add_To_Attr ('IN_CITY',     'FALSE',   adattr_);
   Client_SYS.Add_To_Attr ('ADDRESS1',addr1_,adattr_);
   Client_SYS.Add_To_Attr ('ADDRESS2',addr2_,adattr_);
   Client_SYS.Add_To_Attr ('ZIP_CODE',zip_,adattr_);
   Client_SYS.Add_To_Attr ('COUNTY',county_,adattr_);
   Client_SYS.Add_To_Attr ('STATE',state_,adattr_);
   Client_SYS.Add_To_Attr ('CUSTOMER_ID',cust_id_,adattr_);
   Client_SYS.Add_To_Attr ('CITY',city_,adattr_);
   Client_SYS.Add_To_Attr ('DEFAULT_DOMAIN','TRUE',adattr_);
   Client_SYS.Add_To_Attr ('PARTY_TYPE','Customer',adattr_);
   Customer_Info_Address_API.New__ (info_, objid_, objver_, adattr_, 'DO');

   for x_ in client_addr_types Loop
          Client_SYS.Clear_Attr(adattr_);
          Client_SYS.Add_To_Attr('CUSTOMER_ID',cust_id_, adattr_);
          Client_SYS.Add_To_Attr('ADDRESS_ID',addr_id_, adattr_);
          Client_SYS.Add_To_Attr('ADDRESS_TYPE_CODE',x_.addr_type, adattr_);

          if address_type_ ='INVOICE' then
             if x_.addr_type = 'Document' then
                 Client_SYS.Add_To_Attr('DEF_ADDRESS','TRUE', adattr_);
             else 
                 Client_SYS.Add_To_Attr('DEF_ADDRESS','FALSE', adattr_);
             end if;
          else 
                 Client_SYS.Add_To_Attr('DEF_ADDRESS','TRUE', adattr_);
          end if;

          CUSTOMER_INFO_ADDRESS_TYPE_API.NEW__(info_, objid_, objver_, adattr_ , 'DO');
      end loop;
      Client_SYS.Clear_Attr(adattr_);
      Client_SYS.Add_To_Attr('CUSTOMER_ID',cust_id_,adattr_);
      Client_SYS.Add_To_Attr('ADDRESS_ID',addr_id_,adattr_);
      Client_SYS.Add_To_Attr('COMPANY_NAME2',addr_name_,adattr_);
      Client_SYS.Add_To_Attr('SHIP_VIA_CODE','003',adattr_);
      Client_SYS.Add_To_Attr('DELIVERY_TERMS','EXW',adattr_);
      Client_SYS.Add_To_Attr('INTRASTAT_EXEMPT_DB','INCLUDE',adattr_);
      Client_SYS.Add_To_Attr('INTRASTAT_EXEMPT','Include',adattr_);
      CUST_ORD_CUSTOMER_ADDRESS_API.NEW__(info_, objid_, objver_, adattr_ , 'DO');
      
      Client_SYS.Clear_Attr(adattr_);
      Client_SYS.Add_To_Attr('CUSTOMER_ID',cust_id_,adattr_);
      Client_SYS.Add_To_Attr('ADDRESS_ID',addr_id_,adattr_);
      Client_SYS.Add_To_Attr('COMPANY', checkit_company_,adattr_);
      Client_SYS.Add_To_Attr('TAX_REGIME_DB',Tax_Regime_API.Encode( 'VAT' ),adattr_);
      Client_SYS.Add_To_Attr('LIABILITY_TYPE','TAX',adattr_);
      Client_SYS.Add_To_Attr('TAX_WITHHOLDING_DB',Cust_Tax_Withhold_API.Encode( 'Blocked' ),adattr_);
      Client_SYS.Add_To_Attr('VAT_FREE_VAT_CODE',0,adattr_);
      CUSTOMER_INFO_VAT_API.NEW__(info_, objid_, objver_, adattr_ , 'DO');
   --   commit;

      if IDENTITY_INVOICE_INFO_API.Check_Exist(checkit_company_, cust_id_, 'Customer') ='FALSE' then
        Client_SYS.Clear_Attr(adattr_);
        Client_SYS.Add_To_Attr('IDENTITY',cust_id_,adattr_);
        Client_SYS.Add_To_Attr('COMPANY',checkit_company_,adattr_);
        Client_SYS.Add_To_Attr('PARTY_TYPE','Customer', adattr_);
        Client_SYS.Add_To_Attr('IDENTITY_TYPE','External', adattr_);
        Client_SYS.Add_To_Attr('DEF_CURRENCY','GBP',adattr_);
        Client_SYS.Add_To_Attr('GROUP_ID','0',adattr_);
        Client_SYS.Add_To_Attr('NO_INVOICE_COPIES',0,adattr_);
        Client_SYS.Add_To_Attr('PAY_TERM_ID',0,adattr_);
        Client_SYS.Add_To_Attr('INVOICE_FEE','FALSE',adattr_);
        Client_SYS.Add_To_Attr('PRINT_TAX_CODE_TEXT','FALSE',adattr_);
        Client_SYS.Add_To_Attr('PRINT_TAX_CODE_TEXT','FALSE',adattr_);
        IDENTITY_INVOICE_INFO_API.NEW__(info_, objid_, objver_, adattr_ , 'DO');
     --   commit;
        
        Add_Payinfo__(cust_id_);
        
      end if;
      
    END Add_Address___;

    
PROCEDURE Add_New_Address (
  customer_id_ IN VARCHAR2,
  address_country_code_ IN VARCHAR2,
  addr_name_ IN VARCHAR2,
  addr1_ IN VARCHAR2,
  addr2_ IN VARCHAR2,
  city_ IN VARCHAR2,
  county_ IN VARCHAR2,
  state_ IN VARCHAR2,
  zip_ IN VARCHAR2,
  address_type_ IN VARCHAR2 DEFAULT NULL,
  address_id OUT VARCHAR2
  ) IS
  
  addr_id_ varchar2(50);
-----
      hattr1_ VARCHAR2(2000);
      hinfo1_ VARCHAR2(2000);
      hinfo2_ VARCHAR2(2000);
-----
  
BEGIN
    
   --[Hist Info]--
   Client_SYS.Clear_Attr(hattr1_);
   Client_SYS.Add_To_Attr('CUSTOMER_ID',customer_id_,hattr1_);
   Client_SYS.Add_To_Attr('ADDRESS_COUNTRY_CODE',address_country_code_,hattr1_);
   Client_SYS.Add_To_Attr('ADDR_NAME',addr_name_,hattr1_);
   Client_SYS.Add_To_Attr('ADDR1',addr1_,hattr1_);
   Client_SYS.Add_To_Attr('ADDR2',addr2_,hattr1_);
   Client_SYS.Add_To_Attr('CITY',city_,hattr1_);
   Client_SYS.Add_To_Attr('STATE',county_,hattr1_);
   Client_SYS.Add_To_Attr('CUSTOMER_ID',state_,hattr1_);
   Client_SYS.Add_To_Attr('ZIP',zip_,hattr1_);
   Client_SYS.Add_To_Attr('ADDRESS_TYPE',address_type_,hattr1_);
   --[Hist Info]--
  
  -- check current address id 
  if address_type_='INVOICE' then
   addr_id_ := '02';
  else
    addr_id_ := '01';
  end if;
  
  address_id:= addr_id_;
  
  Add_Address___(
          addr_id_, 
          customer_id_,
          address_country_code_,
          addr_name_,
          addr1_,
          addr2_,
          city_,
          county_,
          state_,
          zip_,
          address_type_);

        Client_SYS.Clear_Attr(hinfo1_);
        Client_SYS.Add_To_Attr('ADDRESS_ID', addr_id_, hinfo1_);
        EK_CHECKIT_UTIL_API.Debug_Hist_Log___(info_ => hinfo1_,
                                        attr1_ => hattr1_,
                                        attr2_ => null,
                                        action_ => 'Add_New_Address');
          
        COMMIT;
EXCEPTION
   WHEN OTHERS THEN
      EK_CHECKIT_UTIL_API.Handle_Errors___(info_ => hinfo2_,
                                       attr_ => hinfo1_,
                                       err_code_ => SQLCODE,
                                       err_msg_ => SQLERRM);
      EK_CHECKIT_UTIL_API.Debug_Hist_Log___(info_ =>hinfo1_,
                                        attr1_ => hattr1_,
                                        attr2_ => null,
                                        action_ => 'Add_New_Address');
      RAISE;
        
END Add_New_Address;
  

  PROCEDURE Add_Payinfo__(
    customer_id_ IN VARCHAR2
  ) IS

      info_   VARCHAR2(2000);
      attr_   VARCHAR2(2000);
      objid_  VARCHAR2(2000);
      objver_ VARCHAR2(2000);
  BEGIN
          
          Client_SYS.Clear_Attr(attr_);    
          Client_SYS.Add_To_Attr('COMPANY', checkit_company_, attr_);
          Client_SYS.Add_To_Attr('IDENTITY', customer_id_, attr_);
          Client_SYS.Add_To_Attr('REMINDER_TEMPLATE', '1', attr_);
          Client_SYS.Add_To_Attr('PAYMENT_ADVICE', 'No Advice', attr_);
          Client_SYS.Add_To_Attr('PARTY_TYPE', 'Customer', attr_);    
          Client_SYS.Add_To_Attr('INTEREST_TEMPLATE', '1', attr_);  
          Client_SYS.Add_To_Attr('PAYMENT_RECEIPT_TYPE', 'No Receipt', attr_);  
          Client_SYS.Add_To_Attr('SEND_REMINDER_TO_PAYER', 'FALSE', attr_);  
          Client_SYS.Add_To_Attr('SEND_INTEREST_INV_TO_PAYER', 'FALSE', attr_);  
          Client_SYS.Add_To_Attr('SEND_STATEMENT_OF_ACC_TO_PAYER', 'FALSE', attr_);  
          Client_SYS.Add_To_Attr('NETTING_ALLOWED', 'FALSE', attr_);  
          Client_SYS.Add_To_Attr('SEND_REMINDER_TO_PAYER', 'FALSE', attr_);  
          Client_SYS.Add_To_Attr('SEND_INTEREST_INV_TO_PAYER', 'FALSE', attr_);  
          Client_SYS.Add_To_Attr('SEND_STATEMENT_OF_ACC_TO_PAYER', 'FALSE', attr_);  
                                                            
          IFSAPP.IDENTITY_PAY_INFO_API.NEW__( info_,objid_,objver_, attr_, 'DO' );
        --  commit;
    
  END Add_Payinfo__;


  PROCEDURE Create_customer_header(
    customer_id_ IN VARCHAR2,
    cust_name_ IN VARCHAR2,
    language_ IN VARCHAR2,
    country_ IN VARCHAR2,
    cust_reference_ IN VARCHAR2
  )
  IS

      info_   VARCHAR2(2000);
      attr_   VARCHAR2(2000);
      objid_  VARCHAR2(2000);
      objver_ VARCHAR2(2000);

  BEGIN

      Client_SYS.Clear_Attr(attr_);
      Client_SYS.Add_To_Attr('CUSTOMER_ID',customer_id_,attr_);
      Client_SYS.Add_To_Attr('NAME',cust_name_,attr_);
      Client_SYS.Add_To_Attr('DEFAULT_LANGUAGE',language_,attr_);
      Client_SYS.Add_To_Attr('COUNTRY',country_,attr_);
      Client_SYS.Add_To_Attr('CREATION_DATE',SYSDATE,attr_);
      Client_SYS.Add_To_Attr('DEFAULT_DOMAIN','TRUE',attr_);
      Client_SYS.Add_To_Attr('PARTY_TYPE','Customer',attr_);
      Client_SYS.Add_To_Attr('IDENTIFIER_REFERENCE',cust_reference_,attr_);

      CUSTOMER_INFO_API.NEW__( info_,objid_,objver_, attr_, 'DO' );
     
    --  commit;

END Create_customer_header;


PROCEDURE Customer_Exist (
   cust_name_            IN  VARCHAR2,
   language_             IN  VARCHAR2 DEFAULT NULL,
   cust_country_code_    IN  VARCHAR2 DEFAULT NULL,
   cust_reference_       IN  VARCHAR2 DEFAULT NULL,
   addr_id_              IN  VARCHAR2 DEFAULT NULL, 
   address_country_code_ IN  VARCHAR2 DEFAULT NULL,
   addr_name_            IN  VARCHAR2 DEFAULT NULL,
   addr1_                IN  VARCHAR2 DEFAULT NULL,
   addr2_                IN  VARCHAR2 DEFAULT NULL,
   city_                 IN  VARCHAR2 DEFAULT NULL,
   county_               IN  VARCHAR2 DEFAULT NULL,
   state_                IN  VARCHAR2 DEFAULT NULL,
   zip_                  IN  VARCHAR2 DEFAULT NULL,
   is_exist_             OUT VARCHAR2 )
IS
   CURSOR check_exist IS
      SELECT 1
      FROM   customer_info_tab j
      WHERE  j.name = cust_name_;
   dummy_ NUMBER;
BEGIN
   OPEN  check_exist;
   FETCH check_exist INTO dummy_;
   IF check_exist%FOUND THEN
      is_exist_ := 'TRUE';
   ELSE
      is_exist_ := 'FALSE';
   END IF;
   CLOSE check_exist;
END customer_exist;


FUNCTION Check_Exist___ (
   customer_id_ IN VARCHAR2 ) RETURN BOOLEAN
IS
   dummy_ NUMBER;
   CURSOR exist_control IS
      SELECT 1
      FROM   CUSTOMER_INFO_TAB
      WHERE  customer_id = customer_id_;
BEGIN
   OPEN exist_control;
   FETCH exist_control INTO dummy_;
   IF (exist_control%FOUND) THEN
      CLOSE exist_control;
      RETURN(TRUE);
   END IF;
   CLOSE exist_control;
   RETURN(FALSE);
END Check_Exist___;


PROCEDURE Order_Header(
  customer_no_  IN VARCHAR2,
  checkit_order_type_ IN VARCHAR2,
  coordinator_  IN VARCHAR2, 
  parent_order_ IN VARCHAR2,
  order_no_     OUT VARCHAR2
)
IS

    info_   VARCHAR2(2000);
    attr_   VARCHAR2(2000);
    objid_  VARCHAR2(2000);
    objver_ VARCHAR2(2000);
    
    new_attr_       VARCHAR2(5000);
    order_type_     CUSTOMER_ORDER_TAB.order_id%TYPE;
    currency_code_  CUSTOMER_ORDER_TAB.currency_code%TYPE;
    ship_addr_no_   CUSTOMER_ORDER_TAB.ship_addr_no%TYPE;
    bill_addr_no_   CUSTOMER_ORDER_TAB.bill_addr_no%TYPE;
    auth_code_      VARCHAR2(6);
   
BEGIN
   
    --IF Transaction_SYS.Package_Is_Installed('Customer_Order_API') THEN
    Client_SYS.Clear_Attr(new_attr_);
    Client_SYS.Add_To_Attr('CONTRACT', checkit_site_, new_attr_);
    Client_SYS.Add_To_Attr('ORDER_TYPE', checkit_order_type_, new_attr_);
    Client_SYS.Add_To_Attr('CUSTOMER_NO', customer_no_, new_attr_);
    Customer_Order_API.Get_Customer_Defaults__(new_attr_);
  --  commit;

    ship_addr_no_ := Client_SYS.Get_Item_Value('SHIP_ADDR_NO', new_attr_);
    currency_code_ := Client_SYS.Get_Item_Value('CURRENCY_CODE', new_attr_);
    order_type_ := Client_SYS.Get_Item_Value('ORDER_ID', new_attr_);
    bill_addr_no_ := Client_SYS.Get_Item_Value('BILL_ADDR_NO', new_attr_);
    
    if (coordinator_ is null) then 
        auth_code_ := 'IFSAPP';
     else
        auth_code_ := coordinator_;
     end if;
        
    Client_SYS.Clear_Attr(attr_);
    
    --Client_SYS.Add_To_Attr('ORDER_NO','WASA100',attr_);
    Client_SYS.Add_To_Attr('CONTRACT', checkit_site_, attr_);
    Client_SYS.Add_To_Attr('CUSTOMER_NO',customer_no_,attr_);
    Client_SYS.Add_To_Attr('WANTED_DELIVERY_DATE',sysdate+2,attr_);
    Client_SYS.Add_To_Attr('ORDER_ID',order_type_,attr_);
    Client_SYS.Add_To_Attr('AUTHORIZE_CODE',auth_code_,attr_);
    Client_SYS.Add_To_Attr('SHIP_ADDR_NO', ship_addr_no_, attr_);
    Client_SYS.Add_To_Attr('BILL_ADDR_NO', bill_addr_no_, attr_);
    Client_SYS.Add_To_Attr('CURRENCY_CODE', currency_code_, attr_);
    Client_SYS.Add_To_Attr('CUSTOMER_PO_NO', parent_order_, attr_);

    CUSTOMER_ORDER_API.NEW__( info_,objid_,objver_, attr_, 'DO' );
   
   -- commit;
    
    order_no_ := Client_SYS.Get_Item_Value('ORDER_NO', attr_);
    
    
    Client_SYS.Clear_Attr(attr_);
    
    customer_order_history_api.New(order_no_ ,'checkit E-commerce order.');

END Order_Header;

PROCEDURE Add_Contact(
  customer_id_ IN VARCHAR2,
  com_method_ IN VARCHAR2,
  name_ IN VARCHAR2,
  description_ IN VARCHAR2,
  value_ IN VARCHAR2,
  method_def_ IN VARCHAR2,
  address_id_ IN VARCHAR2 default null
) IS

    info_   VARCHAR2(2000);
    attr_   VARCHAR2(2000);
    objid_  VARCHAR2(2000);
    objver_ VARCHAR2(2000);
BEGIN

        Client_SYS.Clear_Attr(attr_);    
        Client_SYS.Add_To_Attr('PARTY_TYPE_DB', 'CUSTOMER', attr_);
        Client_SYS.Add_To_Attr('IDENTITY', customer_id_, attr_);
        Client_SYS.Add_To_Attr('NAME', name_, attr_);
        Client_SYS.Add_To_Attr('DESCRIPTION', description_, attr_);
        Client_SYS.Add_To_Attr('METHOD_ID', com_method_, attr_);
        Client_SYS.Add_To_Attr('VALUE', value_, attr_);
        Client_SYS.Add_To_Attr('METHOD_DEFAULT', Method_def_, attr_);
        Client_SYS.Add_To_Attr('ADDRESS_ID', address_id_, attr_);
        IFSAPP.COMM_METHOD_API.NEW__( info_,objid_,objver_, attr_, 'DO' );
     --   commit;
  
END Add_Contact;
 
PROCEDURE Order_Line__ (
   order_no_  IN VARCHAR2,
   part_no_   IN VARCHAR2,
   quantity_  IN NUMBER,
   price_     IN NUMBER,
   discount_  IN NUMBER )
IS
   info_                     VARCHAR2(2000);
   attr_                     VARCHAR2(32000);
   objid_                    VARCHAR2(2000);
   rec_                      EXTERNAL_CUST_ORDER_LINE_TAB%ROWTYPE;
   company_                  VARCHAR2(20);
   curr_code_                VARCHAR2(20);
   curr_rate_                NUMBER;
   curr_type_                VARCHAR2(20);
   conv_factor_              NUMBER;
   headrec_                  Customer_Order_API.public_rec;
   ship_addr_no_             CUSTOMER_ORDER_TAB.ship_addr_no%TYPE := NULL;
   head_addr_rec_            Customer_Order_Address_API.public_rec;
   stmt_                     VARCHAR2(2000);
   orig_order_no_            VARCHAR2(12);
   orig_line_no_             VARCHAR2(4);
   orig_rel_no_              VARCHAR2(4);
   orig_line_item_no_        NUMBER;
   demand_code_db_           VARCHAR2(20);
   po_contract_              VARCHAR2(5);
   orig_attr_                VARCHAR2(2000);
   pay_tax_db_               VARCHAR2(15);

   objver_                   VARCHAR2(2000);

BEGIN
   headrec_ := Customer_Order_API.Get (order_no_);

   Client_SYS.Clear_Attr(attr_);
   Client_SYS.Add_To_Attr('ORDER_NO', order_no_, attr_);

   Client_SYS.Add_To_Attr('CONTRACT', checkit_site_, attr_);
   Client_SYS.Add_To_Attr('WANTED_DELIVERY_DATE', sysdate +2 , attr_);
   Client_SYS.Add_To_Attr('PLANNED_DELIVERY_DATE', sysdate +2 , attr_);
   Client_SYS.Add_To_Attr('NOTE_TEXT', 'E-commerce testing' , attr_);
   Client_SYS.Add_To_Attr('DELIVER_TO_CUSTOMER_NO', headrec_.customer_no, attr_);

   -- Lookup the sales part number in the sales part cross reference
   --catalog_no_ := Sales_Part_Cross_Reference_API.Get_Catalog_No(headrec_.customer_no, checkit_site_, part_no_);

   Client_SYS.Add_To_Attr('CATALOG_NO',             part_no_ ,  attr_);
   Client_SYS.Add_To_Attr('PART_NO',                Sales_Part_API.Get_Part_No(checkit_site_, part_no_) , attr_);
   Client_SYS.Add_To_Attr('PURCHASE_PART_NO',       Sales_Part_API.Get_Part_No(checkit_site_, part_no_) , attr_);
   Client_SYS.Add_To_Attr('CUSTOMER_PART_NO',       part_no_ ,  attr_);
   Client_SYS.Add_To_Attr ('BUY_QTY_DUE',           quantity_ , attr_);
   Client_SYS.Add_To_Attr ('CUSTOMER_PART_BUY_QTY', quantity_ , attr_);

   IF (price_ IS NOT NULL) THEN
      company_   := Site_API.Get_Company(headrec_.contract);
      curr_code_ := 'GBP';--External_Customer_Order_API.Get_Currency_Code(message_id_);
      Invoice_Library_API.Get_Currency_Rate_Defaults(
         currency_type_ => curr_type_,
         conv_factor_   => conv_factor_,
         currency_rate_ => curr_rate_,
         company_       => company_,
         currency_code_ => curr_code_,
         date_          => Site_API.Get_Site_Date(headrec_.contract),
         related_to_    => 'CUSTOMER',
         identity_      => NVL(headrec_.customer_no_pay, headrec_.customer_no) );
      curr_rate_ := curr_rate_ / conv_factor_;

      --Client_SYS.Add_To_Attr('VENDOR_NO', curr_code_, attr_);
      --Client_SYS.Add_To_Attr('CURRENCY_CODE', curr_code_, attr_);
      Client_SYS.Add_To_Attr ('CURRENCY_RATE', curr_rate_, attr_);
      Client_SYS.Add_To_Attr ('SALE_UNIT_PRICE', price_, attr_);
      Client_SYS.Add_To_Attr ('BASE_SALE_UNIT_PRICE', price_ * curr_rate_, attr_);
      Client_SYS.Add_To_Attr ('CUSTOMER_PART_UNIT_MEAS','pcs',attr_);
      Client_SYS.Add_To_Attr ('SALES_UNIT_MEAS','pcs',attr_);
      -- price freeze flag need to be set after creation of the customer order line
   END IF;

   IF (discount_ IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('DISCOUNT', discount_, attr_);
   END IF;

   IF rec_.catalog_desc IS NOT NULL THEN
      Client_SYS.Add_To_Attr('CATALOG_DESC', rec_.catalog_desc, attr_);
   END IF;

   --Client_SYS.Add_To_Attr('CATALOG_TYPE', Sales_Part_Type_API.Encode(Sales_Part_API.Get_Catalog_Type_Db(headrec_.contract, part_no_)), attr_);
   Client_SYS.Add_To_Attr('CATALOG_TYPE', 'Inventory part', attr_);
   Client_SYS.Add_To_Attr('CATALOG_TYPE_DB', 'INV', attr_);
      
   Client_SYS.Add_To_Attr('CONV_FACTOR', 1, attr_);
   Client_SYS.Add_To_Attr('COST', 0, attr_);
   Client_SYS.Add_To_Attr('PRICE_CONV_FACTOR', 1, attr_);
   Client_SYS.Add_To_Attr('QTY_ASSIGNED', 0, attr_);
   Client_SYS.Add_To_Attr('REVISED_QTY_DUE', 1, attr_);
   Client_SYS.Add_To_Attr('SUPPLY_CODE', 'Purch Order Trans', attr_);
   Client_SYS.Add_To_Attr('CONSIGNMENT_STOCK', 'No Consignment Stock', attr_);
   Client_SYS.Add_To_Attr('CLOSE_TOLERANCE', 0, attr_);
   Client_SYS.Add_To_Attr('CREATE_SM_OBJECT_OPTION', 'Do not create SM object', attr_);
   Client_SYS.Add_To_Attr('PART_PRICE', price_, attr_);
   Client_SYS.Add_To_Attr('PRICE_SOURCE', 'Base', attr_);
   Client_SYS.Add_To_Attr('PRICE_FREEZE', 'Free', attr_);
   Client_SYS.Add_To_Attr('CONFIGURATION_ID', '*', attr_);
   Client_SYS.Add_To_Attr('SUPPLY_SITE_RESERVE_TYPE', 'Not Allowed', attr_);

   --INTRASTAT_EXEMPT
   --Intrastat_Exempt_API.Encode(value_)
   
  /*IF (rec_.forward_agent_id IS NOT NULL) THEN
     Client_SYS.Add_To_Attr('FORWARD_AGENT_ID', rec_.forward_agent_id, attr_);
   END IF;*/

   /*IF (rec_.condition_code IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('CONDITION_CODE', rec_.condition_code, attr_);
   END IF;*/

   IF (headrec_.contract IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('CONTACT', headrec_.contract, attr_);
   END IF;   

   ship_addr_no_ := Cust_Ord_Customer_Address_API.Get_Id_By_Ean_Location(nvl(headrec_.customer_no, headrec_.customer_no), rec_.ean_location_del_addr);

   --IF rec_.internal_delivery_type = 'INTDIRECT' THEN
      demand_code_db_ := NULL;
   --ELSIF rec_.internal_delivery_type = 'INTTRANSIT' THEN
   --   demand_code_db_ := 'IPT';
   --END IF;

   IF (demand_code_db_ = 'IPD') AND (ship_addr_no_ IS NULL) AND (rec_.ean_location_del_addr IS NOT NULL) THEN
      Error_SYS.Record_General(lu_name_, 'ECOLNOMATCH: Delivery address cannot be found for the customer :P1 and own address :P2',
                               rec_.deliver_to_customer_no, rec_.ean_location_del_addr);
   END IF;

   -- When IPD and Single occurrence, ean_location_del_addr and deliver_to_customer_no always null
   --IF (rec_.ean_location_del_addr IS NULL) THEN
      Client_SYS.Add_To_Attr('ADDR_FLAG_DB', 'Y', attr_);
   --ELSE
     -- Client_SYS.Add_To_Attr('ADDR_FLAG_DB', 'N', attr_);
   --END IF;

  /* OPEN get_header_ean_deliv;
   
   FETCH get_header_ean_deliv
   INTO  header_ean_deliv_,
         internal_customer_site_,
         header_ship_via_,
         header_del_terms_,
         header_forward_agent_id_,
         header_del_terms_loc_;

   CLOSE get_header_ean_deliv;
   
   IF (ship_addr_no_ IS NOT NULL ) THEN
      Client_SYS.Add_To_Attr('SHIP_ADDR_NO', ship_addr_no_, attr_);
      -- Bug 74960, Added NVL for del_terms_location in IF condition
      -- Bug 70005, Added check for del_terms_location in IF condition.
      IF (header_ean_deliv_ IS NOT NULL) THEN
         IF (header_ean_deliv_                  != rec_.ean_location_del_addr )
         OR (header_ship_via_                   != rec_.ship_via_code)
         OR (header_del_terms_                  != rec_.delivery_terms)
         OR Order_Delivery_Term_API.Get_Description(header_del_terms_) != Order_Delivery_Term_API.Get_Description(rec_.delivery_terms)
         OR (nvl(header_forward_agent_id_,' ')  != rec_.forward_agent_id)
         OR (NVL(header_del_terms_loc_,' ')     != NVL(rec_.del_terms_location,' ')) THEN

            Client_SYS.Add_To_Attr('DEFAULT_ADDR_FLAG_DB', 'N', attr_);
         END IF;
      END IF;
   END IF;
   
    */
   

   --For internal derect deliveries, order lines should not be order default addresses.
   IF (demand_code_db_ = 'IPD') THEN
      Client_SYS.Set_Item_Value('DEFAULT_ADDR_FLAG_DB', 'N', attr_);
   END IF;

-- this is special condition wasa
   --IF  (((rec_.ship_via_code IS NOT NULL) OR (rec_.delivery_leadtime IS NOT NULL)) AND ship_addr_no_ IS NULL) THEN
      -- Default_addr_flag must be 'N' to be able to keep the passed values for
      -- ship_via_code and delivery_leadtime.
      Client_SYS.Add_To_Attr('DEFAULT_ADDR_FLAG_DB', 'N', attr_);
      -- Now we have to add some values from the header not passed on line.

      IF (rec_.ship_via_code IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('SHIP_VIA_CODE', rec_.ship_via_code, attr_);
      END IF;
      IF (rec_.delivery_leadtime IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('DELIVERY_LEADTIME', rec_.delivery_leadtime, attr_);
      END IF;
      IF (rec_.delivery_terms IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('DELIVERY_TERMS', rec_.delivery_terms, attr_);
      END IF;


      IF (rec_.del_terms_location IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('DEL_TERMS_LOCATION', rec_.del_terms_location, attr_);
      END IF;

      IF (rec_.route_id IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('ROUTE_ID', rec_.route_id, attr_);
      END IF;
      IF (headrec_.ship_addr_no IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('SHIP_ADDR_NO', headrec_.ship_addr_no, attr_);
      END IF;
      IF (headrec_.district_code IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('DISTRICT_CODE', headrec_.district_code, attr_);
      END IF;
      IF (headrec_.region_code IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('REGION_CODE', headrec_.region_code, attr_);
      END IF;
      IF (headrec_.vat IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('VAT_DB', headrec_.vat, attr_);
      END IF;
      IF (headrec_.intrastat_exempt IS NOT NULL) THEN
         Client_SYS.Add_To_Attr('INTRASTAT_EXEMPT_DB', 'INCLUDE', attr_);
      END IF;
      
--   Client_SYS.Add_To_Attr('INTRASTAT_EXEMPT', 'Include', attr_);
      

      IF (rec_.delivery_address_name IS NULL) AND (rec_.ship_address1 IS NULL) AND
         (rec_.ship_address2 IS NULL) AND (rec_.ship_zip_code IS NULL) AND
         (rec_.ship_city IS NULL) AND (rec_.ship_state IS NULL) AND
         (rec_.country_code IS NULL) AND (rec_.ship_county IS NULL) AND (headrec_.addr_flag = 'Y') THEN
         -- No address has been passed but the order address is a single occurance
         -- so the address needs to be fetched from the order head address.
         rec_.delivery_address_name := head_addr_rec_.addr_1;
         rec_.ship_address1         := head_addr_rec_.address1;
         rec_.ship_address2         := head_addr_rec_.address2;
         rec_.ship_zip_code         := head_addr_rec_.zip_code;
         rec_.ship_city             := head_addr_rec_.city;
         rec_.ship_state            := head_addr_rec_.state;
         rec_.country_code          := head_addr_rec_.country_code;
         rec_.ship_county           := head_addr_rec_.county;
         rec_.in_city               := head_addr_rec_.in_city;
      END IF;
      IF ((rec_.delivery_address_name IS NOT NULL) OR (rec_.ship_address1 IS NOT NULL) OR
         (rec_.ship_address2 IS NOT NULL) OR (rec_.ship_zip_code IS NOT NULL) OR
         (rec_.ship_city IS NOT NULL) OR (rec_.ship_state IS NOT NULL) OR
         (rec_.country_code IS NOT NULL) OR (rec_.ship_county IS NOT NULL)) AND (ship_addr_no_ IS NULL) THEN
         -- Single occurence address
         Client_SYS.Add_To_Attr('ADDR_FLAG_DB', 'Y', attr_);
         IF (rec_.ean_location_del_addr IS NULL) THEN
            Client_SYS.Add_To_Attr('SHIP_ADDR_NO', (Customer_Info_Address_API.Get_Default_Address(NVL(rec_.deliver_to_customer_no, headrec_.customer_no), Address_Type_Code_API.Decode('DELIVERY'))), attr_);
         END IF;
      ELSE
         Client_SYS.Add_To_Attr('ADDR_FLAG_DB', headrec_.addr_flag, attr_);
      END IF;

   --END IF;

   pay_tax_db_  := Taxable_API.Encode(Tax_Liability_API.Get_Taxable(rec_.vat_pay_tax, rec_.country_code));

   --Internal PO derect. Pass the values came from the original CO.
   IF (demand_code_db_ = 'IPD') THEN
      Client_SYS.Set_Item_Value('DELIVERY_LEADTIME', rec_.delivery_leadtime, attr_);
      Client_SYS.Set_Item_Value('ROUTE_ID', rec_.route_id, attr_);
      Client_SYS.Set_Item_Value('DISTRICT_CODE', rec_.district_code, attr_);
      Client_SYS.Set_Item_Value('REGION_CODE', rec_.region_code, attr_);
      Client_SYS.Set_Item_Value('INTRASTAT_EXEMPT_DB', 'INCLUDE', attr_);

      IF (ship_addr_no_ IS NOT NULL) THEN
         IF (pay_tax_db_ IN ('TAX','RDE')) THEN
            Client_SYS.Add_To_Attr('VAT_DB', 'Y' , attr_);
         ELSIF (pay_tax_db_ = 'EXM') THEN
            Client_SYS.Add_To_Attr('VAT_DB', 'N' , attr_);
            Client_SYS.Add_To_Attr('FEE_CODE', rec_.vat_free_vat_code, attr_);
         END IF;
      END IF;

      Client_SYS.Add_To_Attr('VAT_NO', rec_.vat_no, attr_);
   END IF;

   --Client_SYS.Add_To_Attr('DEMAND_CODE_DB', demand_code_db_, attr_);
   IF (headrec_.internal_po_no IS NOT NULL) THEN
      Client_SYS.Add_To_Attr('DEMAND_ORDER_REF1', headrec_.internal_po_no, attr_);
   END IF;
   Client_SYS.Add_To_Attr('DEMAND_ORDER_REF2', rec_.line_no, attr_);
   Client_SYS.Add_To_Attr('DEMAND_ORDER_REF3', rec_.rel_no, attr_);

   Client_SYS.Add_To_Attr('SHIP_VIA_CODE', rec_.ship_via_code, attr_);
   Client_SYS.Add_To_Attr('DELIVERY_TERMS', rec_.delivery_terms, attr_);
   Client_SYS.Add_To_Attr('DEL_TERMS_LOCATION', rec_.del_terms_location, attr_);


   IF inst_PurchaseOrderLine_ AND (headrec_.internal_po_no IS NOT NULL) THEN
      -- get the originating order line keys
      -- Fetch the Internal PO site.
      stmt_ := 'DECLARE
                   poline_ Purchase_Order_Line_API.Public_Rec;
                BEGIN
                   poline_ := Purchase_Order_Line_API.Get(:po_order_no, :po_line_no, :po_release_no);
                   IF (poline_.demand_code IN (''ICD'', ''ICT'')) THEN
                      :oe_order_no := poline_.demand_order_no;
                      :oe_line_no := poline_.demand_release;
                      :oe_rel_no := poline_.demand_sequence_no;
                      :oe_line_item_no := Purchase_Order_Line_API.Get_Demand_Operation_No(:po_order_no, :po_line_no, :po_release_no);
                      :po_contract := poline_.contract;
                   END IF;
                END;';
      EXECUTE IMMEDIATE stmt_ USING IN headrec_.internal_po_no, IN rec_.line_no, IN rec_.rel_no,
            OUT orig_order_no_, OUT orig_line_no_, OUT orig_rel_no_, OUT orig_line_item_no_,OUT po_contract_;

      IF (orig_order_no_ IS NOT NULL AND orig_line_no_ IS NOT NULL AND orig_rel_no_ IS NOT NULL AND orig_line_item_no_ IS NOT NULL)THEN
         Client_SYS.Clear_Attr(orig_attr_ );
         Client_SYS.Add_To_Attr('RELEASE_PLANNING_DB' , 'NOTRELEASED' , orig_attr_);
         Customer_Order_Line_API.Modify(orig_attr_, orig_order_no_, orig_line_no_, orig_rel_no_, orig_line_item_no_);
      END IF;
   END IF;


    CUSTOMER_ORDER_LINE_API.NEW__( info_,objid_,objver_, attr_, 'DO' );
   
   -- commit;
    
    Client_SYS.Clear_Attr(attr_);

END Order_Line__;

PROCEDURE New_Order_Line(
  order_no_ IN VARCHAR2,
  part_no_ IN VARCHAR2,
  quantity_ IN NUMBER,
  price_ IN NUMBER,
  discount_ IN NUMBER
)
IS

BEGIN
  
  Order_line__(order_no_,
  part_no_,
  quantity_,
  price_,
  discount_);

 -- commit;

END New_Order_Line;

  
PROCEDURE New_Order_with_line(
  order_no_ OUT VARCHAR2,
  customer_no_ IN VARCHAR2,
  order_type_ IN VARCHAR2,
  coordinator_ IN VARCHAR2, 
  parent_order_ IN VARCHAR2, 
  part_no_ IN VARCHAR2,
  quantity_ IN NUMBER,
  price_ IN NUMBER,
  discount_ IN NUMBER
  ) 
IS

BEGIN
  
    Order_Header( 
        customer_no_,
        order_type_,        
        coordinator_,
        parent_order_,
        order_no_);
      
    Order_Line__(
        order_no_, 
        part_no_,
        quantity_,
        price_,
        discount_);
    
       -- commit;

END New_Order_with_line;


/*  
PROCEDURE New_Order_bulk(
  order_no_ OUT VARCHAR2,
  customer_no_ IN VARCHAR2,
  part_no_ IN VARCHAR2,
  quantity_ IN NUMBER,
  price_ IN NUMBER,
  discount_ IN NUMBER
  ) 
IS

BEGIN
  
    Order_Header( 
        customer_no_,
        order_no_);
      
    Order_Line__(
        order_no_, 
        part_no_,
        quantity_,
        price_,
        discount_);
    
        commit;

END New_Order_bulk;*/

PROCEDURE Order_Release(
  order_no_ IN VARCHAR2,
  woo_order_no_ IN VARCHAR2) 
IS BEGIN
   Customer_order_flow_api.Release_Order (order_no_);
   Add_Technical_Chars(order_no_, 'CKCO');
   Update_Technical_Chars(order_no_, 'CKCO', 'CONTRACT TYPE', Woo_order_no_); -- does for all attributes 
END Order_Release;

PROCEDURE Add_Technical_Chars (
   order_no_        IN VARCHAR2,
   technical_class_ IN VARCHAR2 )
IS
   CURSOR spec_no IS
      SELECT technical_spec_no_seq.NEXTVAL
      FROM DUAL;
      
   CURSOR get_spec (spec_no__ NUMBER, attribute__ VARCHAR2) IS
      SELECT *
      FROM   technical_specification_both
      WHERE  technical_spec_no = spec_no__
        AND  technical_class=technical_class_
        AND  attribute = attribute__;

   info_           VARCHAR2(2000);
   attr_           VARCHAR2(2000);
   objid_          VARCHAR2(2000);
   objver_         VARCHAR2(2000);
   spec_no_        NUMBER;
      
BEGIN
  
   OPEN spec_no;
   FETCH spec_no INTO spec_no_;
   CLOSE spec_no;
   
    Client_SYS.Clear_Attr(attr_); 
    Client_SYS.Add_To_Attr('LU_NAME','CustomerOrder',attr_); 
    Client_SYS.Add_To_Attr('KEY_REF','ORDER_NO='|| order_no_||'^',attr_); 
    Client_SYS.Add_To_Attr('KEY_VALUE',order_no_||'^',attr_); 
    Client_SYS.Add_To_Attr('TECHNICAL_CLASS',technical_class_,attr_); 
    Client_SYS.Add_To_Attr('OK_YES_NO',Technical_Obj_Ref_Approved_API.DECODE('1'),attr_); 
    Client_SYS.Add_To_Attr('OK_SIGN', Fnd_Session_API.Get_Fnd_User(), attr_ );
    Client_SYS.Add_To_Attr('DT_OK', sysdate, attr_ );
    Client_SYS.Add_To_Attr('TECHNICAL_SPEC_NO', spec_no_, attr_ );
      
    TECHNICAL_OBJECT_REFERENCE_API.NEW__( info_,objid_,objver_, attr_, 'DO' ); 
   -- commit;
    
    for spec_rec_ in get_spec(spec_no_, 'CONTRACT TYPE') Loop
        Client_SYS.Clear_Attr(attr_); 
        Client_SYS.Add_To_Attr('VALUE_TEXT', 'Peace of Mind', attr_ ); 
        IFSAPP.TECHNICAL_SPECIFICATION_API.Modify__( info_,spec_rec_.objid, spec_rec_.objversion, attr_, 'DO' ); 
        --commit;
    end loop;
     
     customer_order_history_api.New(order_no_ ,'Technical characteristics created.');
     
END Add_Technical_Chars;

PROCEDURE Update_Technical_Chars (
   order_no_        IN VARCHAR2,
   technical_class_ IN VARCHAR2,
   attribute_       IN VARCHAR2,
   woo_order_no_    IN VARCHAR2 )
IS
   CURSOR spec_no IS
      SELECT technical_spec_no
      FROM   technical_object_reference
      WHERE  technical_class=technical_class_
        AND  replace(key_value, '^', '') = order_no_;
           
   spec_no_ NUMBER;
   order_type_ VARCHAR2(5);
   contract_type_  VARCHAR2(25);
   original_co_ VARCHAR2(25);
BEGIN
   OPEN spec_no;
   FETCH spec_no INTO spec_no_;
   CLOSE spec_no;
   order_type_ := ifsapp.customer_order_api.Get_Order_Id(order_no_);
   IF order_type_='CKT' THEN
      contract_type_ := 'Peace of Mind';
      original_co_ := 'ORIGINAL';
   ELSIF order_type_='CKP' THEN
      contract_type_ := 'Peace of Mind';
      original_co_ := order_no_;
   ELSE
      contract_type_ := 'No Ties';
      original_co_ := 'ORIGINAL';
   END IF;
   Update_spec__(spec_no_, 'CONTRACT TYPE', contract_type_);
   Update_spec__(spec_no_, 'ORIGINAL CO', original_co_);
   Update_spec__(spec_no_, 'WOO CO NO', woo_order_no_);
END Update_Technical_Chars;


PROCEDURE Update_spec__ (
   spec_no_         IN NUMBER,
   attribute_       IN VARCHAR2,
   attribute_value_ IN VARCHAR2 )
IS BEGIN
   UPDATE technical_specification_tab
   SET    value_text = attribute_value_
   WHERE  technical_spec_no = spec_no_
     AND  attribute = attribute_;
END Update_spec__;
    
PROCEDURE Create_Relationship( 
  contract_ IN VARCHAR2,
  Customer_no_   IN VARCHAR2, 
  order_no_ IN VARCHAR2) 
IS 

   CURSOR check_exist(order_no_ IN VARCHAR2)  IS
      SELECT 1
      FROM   FA_OBJECT
      WHERE  object_id = order_no_;
      
    info_   VARCHAR2(2000); 
    attr_   VARCHAR2(2000); 
    objid_  VARCHAR2(2000); 
    objver_ VARCHAR2(2000); 
    
    mch_code_ varchar(100); 
    mch_name_ varchar(45); 
    obj_level_ varchar(30); 
    customer_id_ varchar(20); 
    site_   varchar(5);
     
BEGIN 
   
    customer_id_ := customer_no_;

    site_:= checkit_site_;
    
    mch_code_ := 'CKT-' || customer_id_ || '-' ||order_no_; 
    mch_name_ := customer_id_ || ' - ' || customer_info_address_api.Get_City(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)) || '-' || customer_info_address_api.Get_Zip_Code(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_));    
    obj_level_ :='CUSTOMER SITE'; 
 
    Client_SYS.Clear_Attr(attr_); 
    Client_SYS.Add_To_Attr('MCH_CODE',mch_code_,attr_); 
    Client_SYS.Add_To_Attr('MCH_NAME',mch_name_,attr_); 
    Client_SYS.Add_To_Attr('CONTRACT',site_,attr_); 
    Client_SYS.Add_To_Attr('OBJ_LEVEL',obj_level_,attr_); 
    Client_SYS.Add_To_Attr('SUP_CONTRACT',contract_,attr_); 
    Client_SYS.Add_To_Attr('MCH_TYPE','CHEKIT',attr_);

    FOR i_ in check_exist (order_no_) LOOP
           Client_SYS.Add_To_Attr('OBJECT_NO',order_no_,attr_); 
    END LOOP;
    
    EQUIPMENT_FUNCTIONAL_API.NEW__( info_,objid_,objver_, attr_, 'DO' ); 
    
    customer_order_history_api.New(order_no_ ,'Functional Object created: '||mch_code_ );    

 --add record to party tab
    Client_SYS.Clear_Attr(attr_);  
    Client_SYS.Add_To_Attr('CONTRACT',site_,attr_);    
    Client_SYS.Add_To_Attr('MCH_CODE',mch_code_,attr_); 
    Client_SYS.Add_To_Attr('IDENTITY',customer_id_,attr_); 
    Client_SYS.Add_To_Attr('PARTY_TYPE','Customer',attr_);
       
    
    EQUIPMENT_OBJECT_PARTY_API.NEW__( info_,objid_,objver_, attr_, 'DO' ); 
    
-- Add address for object   
    Client_SYS.Clear_Attr(attr_);  
    Client_SYS.Add_To_Attr('CONTRACT',site_,attr_);    
    Client_SYS.Add_To_Attr('MCH_CODE',mch_code_,attr_); 
    Client_SYS.Add_To_Attr('ADDRESS_ID',customer_order_api.Get_Ship_Addr_No(order_no_),attr_); 
    Client_SYS.Add_To_Attr('DEF_ADDRESS','Yes',attr_);
    Client_SYS.Add_To_Attr('ADDRESS1',customer_info_address_api.Get_Address1(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_);    
    Client_SYS.Add_To_Attr('ADDRESS2',customer_info_address_api.Get_Address2(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_); 
    Client_SYS.Add_To_Attr('ADDRESS3',customer_info_address_api.Get_Zip_Code(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_); 
    Client_SYS.Add_To_Attr('ADDRESS4',customer_info_address_api.Get_City(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_);
    Client_SYS.Add_To_Attr('ADDRESS5',customer_info_address_api.Get_State(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_);    
    Client_SYS.Add_To_Attr('ADDRESS6',customer_info_address_api.Get_Country(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_); 
    Client_SYS.Add_To_Attr('ADDRESS7',customer_info_address_api.Get_Country_Code(customer_id_, customer_order_api.Get_Ship_Addr_No(order_no_)),attr_); 
        
    EQUIPMENT_OBJECT_ADDRESS_API.NEW__( info_,objid_,objver_, attr_, 'DO' ); 

END Create_Relationship; 
 

  PROCEDURE Init IS
  BEGIN
    NULL;
  END Init;
END EK_CHECKIT_API;
/
