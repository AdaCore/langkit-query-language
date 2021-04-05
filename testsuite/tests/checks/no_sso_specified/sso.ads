with System;
package SSO is

   type Rec1 is record     --  FLAG
      I : Integer;
   end record;

   for Rec1 use record
      I at 0 range 0 .. 31;
   end record;

   type Rec2 is record      --  NO FLAG
      I : Integer;
   end record;

   for Rec2 use record
      I at 0 range 0 .. 31;
   end record;
   for Rec2'Scalar_Storage_Order use System.Low_Order_First;

   type Tag1 is tagged record     --  FLAG
      I : Integer;
   end record;

   for Tag1 use record
      I at 0 range 0 .. 31;
   end record;

   type Tag2 is new Tag1 with record   --  FLAG
      J : Integer;
   end record;

   for Tag2 use record
      J at 4 range 0 .. 31;
   end record;

   type Tag3 is new Tag1 with null record;   --  NO FLAG
   for Tag3'Scalar_Storage_Order use System.Low_Order_First;

   type Tag4 is new Tag3 with record   --  NO FLAG
      J : Integer;
   end record;

   for Tag4 use record
      J at 4 range 0 .. 31;
   end record;

end SSO;
