with System;
with Ada.Unchecked_Conversion;

procedure P is
   type Byte_Array is array (Natural range <>) of Byte;

   type Foo_Type is delta 0.25 range -1000.0 .. 1000.0;
   for Foo_Type'Small use 0.25;
   for Foo_Type'Size use 16;

   type Big_Endian_Foo_Type is record
      Value : Foo_Type;
   end record;
   for Big_Endian_Foo_Type'Bit_Order use System.High_Order_First;
   for Big_Endian_Foo_Type'Scalar_Storage_Order use System.High_Order_First;

   Raw_Data : Byte_Array := (16#01#, 16#90#);

   Foo_1 : Big_Endian_Foo_Type with   --  FLAG
     Address => Raw_Data'Address;

   function Conv is new Ada.Unchecked_Conversion   --  FLAG
     (Source => Byte_Array, Target => Big_Endian_Foo_Type);

   Foo_2 : Big_Endian_Foo_Type := Conv (Raw_Data);

begin
   null;
end P;
