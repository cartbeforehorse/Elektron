----------------------------------------------------------------
-- Created on 03/07/2015 by OAPGARTH
--
-- To prevent shipment of parts after a user changes a status
-- of Inventory Part to or from "S - "Product Stopped"
--
--
CREATE OR REPLACE TRIGGER ek_ip_status_updates_so_u
   AFTER UPDATE OF part_status ON inventory_part_tab
   FOR EACH ROW
DECLARE
   args_        VARCHAR2(2000);
   job_         VARCHAR2(34)  := 'Ek_Event_Logic_API.Park_Shop_Order';
   desc_        VARCHAR2(200) := 'Modify Shop Order status after update of Inventory Part to/from status "S"';
BEGIN
   Client_SYS.Clear_Attr (args_);
   Client_SYS.Add_To_Attr ('PART_NO_',    :new.part_no,     args_);
   Client_SYS.Add_To_Attr ('CONTRACT_',   :new.contract,    args_);
   Client_SYS.Add_To_Attr ('NEW_STATUS_', :new.part_status, args_);
   Client_SYS.Add_To_Attr ('OLD_STATUS_', :old.part_status, args_);
   Transaction_SYS.Deferred_Call ( job_, 'PARAMETER', args_, desc_);
END ek_ip_status_updates_so_u;
/
