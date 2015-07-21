------------------------------------------------------------------
-- Created on 09/07/2015 by OAPGARTH
--
-- To prevent parts with status "S - Product Stopped" from being
-- connected to a shipment (thereby preventing the blocked part
-- from being delivered to customer
--
-- Name      Date      Description
-- -------   --------  -------------------------------------------
-- OAPGARTH  20150709  Created
--
------------------------------------------------------------------
CREATE OR REPLACE TRIGGER ek_shipment_order_line_i
   AFTER INSERT ON shipment_order_line_tab
   FOR EACH ROW
DECLARE
   part_status_   inventory_part_tab.part_status%TYPE;
   CURSOR get_co_line_part_status IS
      SELECT Inventory_Part_API.Get_Part_Status (l.contract, l.part_no) part_status
      FROM   customer_order_line_tab l
      WHERE  l.order_no = :new.order_no
        AND  l.line_no = :new.line_no
        AND  l.rel_no = :new.rel_no
        AND  l.line_item_no = :new.line_item_no;
BEGIN
   OPEN  get_co_line_part_status;
   FETCH get_co_line_part_status INTO part_status_;
   CLOSE get_co_line_part_status;
   IF part_status_ = 'S' THEN
      Error_SYS.Record_General ('ShipmentOrderLine', 'PARTBLOCKED: The part on this CO line is marked as STOP. You may not ship this part.');
   END IF;
END ek_shipment_order_line_i;
/
