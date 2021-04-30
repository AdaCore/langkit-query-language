with Ada.Unchecked_Conversion;
with System;
package Conv is
   type My_Address is new System.Address;

   type My_Integer is new Integer;
   type My_Access is access all My_Integer;

   function Address_To_Access is new Ada.Unchecked_Conversion    --  FLAG
     (Source => My_Address,
      Target => My_Access);

   function Address_To_Access2 is new Ada.Unchecked_Conversion  --  FLAG
     (Target => My_Access,
      Source => System.Address);
end Conv;
