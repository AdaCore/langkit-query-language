with System;

package P is
   type T is null record;
   for T'Scalar_Storage_Order use System.High_Order_First;

   type T2 is tagged record
      Field1 : T;          --  FLAG
      Field2 : Integer;
   end record;
end;
