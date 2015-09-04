CREATE OR REPLACE PACKAGE ek_event_logic_api IS

   ------------------------------------------------------------
   -- Author  : OAPGARTH
   -- Created : 03/07/2015 15:24:01
   -- Purpose : To manage custom even logic

   -------------------------------------
   -- Triggered when an inventory part status
   -- is changed to or from "S - Product Stopped"
   PROCEDURE Park_Shop_Order (
      part_no_      IN VARCHAR2,
      contract_     IN VARCHAR2,
      new_status_   IN VARCHAR2,
      old_status_   IN VARCHAR2 );

END ek_event_logic_api;
/
CREATE OR REPLACE PACKAGE BODY ek_event_logic_api IS


   /*************************************************
    * Author  : OAPGARTH
    * Created : 03/07/2015 15:24:01
    *
    * Triggered when an inventory part status
    * is changed to or from "S - Product Stopped"
    */
   PROCEDURE Park_Shop_Order (
      part_no_      IN VARCHAR2,
      contract_     IN VARCHAR2,
      new_status_   IN VARCHAR2,
      old_status_   IN VARCHAR2 )
   IS
      attr_        VARCHAR2(2000);
      CURSOR get_part_rel_shop_orders IS
         SELECT *
         FROM   shop_ord_tab s
         WHERE  s.rowstate IN ('Planned','Released')
          AND   s.contract = contract_
          AND   s.part_no = part_no_;
      CURSOR get_part_parked_shop_orders IS
         SELECT *
         FROM   shop_ord_tab s
         WHERE  s.rowstate IN ('Parked')
          AND   s.contract = contract_
          AND   s.part_no = art_no_;
   BEGIN
      IF new_status_ = 'S' THEN
         FOR s_ IN get_part_rel_shop_orders LOOP
            Client_SYS.Clear_Attr (attr_);
            Shop_Ord0_API.Finite_State_Machine___ (s_, 'Park', attr_);
         END LOOP;
      ELSIF old_status_ = 'S' THEN
         FOR s_ IN get_part_parked_shop_orders LOOP
            Client_SYS.Clear_Attr (attr_);
            Shop_Ord0_API.Finite_State_Machine___ (s_, 'Release', attr_);
         END LOOP;
      END IF;
   END Park_Shop_Order;

END ek_event_logic_api;
/
