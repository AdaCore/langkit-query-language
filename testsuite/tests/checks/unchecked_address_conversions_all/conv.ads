with Ada.Unchecked_Conversion;
with System;
package Conv is
   type My_Address is new System.Address;

   type My_Integer is new Integer;
   type My_Access is access all My_Integer;

   function Address_To_Access is new Ada.Unchecked_Conversion    --  FLAG
     (My_Address, My_Access);

   function Address_To_Access2 is new Ada.Unchecked_Conversion  --  FLAG
     (My_Access, System.Address);
end Conv;
