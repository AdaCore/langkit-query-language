package Simple is

   type Element is record
      Field : Integer := 0;
   end record;

   Max : constant := 1;

   Max_Length : constant Positive := Max;
   subtype Index_Range is Natural range 0 .. Max_Length - 1;

   type Element_Array is array (Index_Range range <>) of Element;
   Null_Element_Array : Element_Array (1 .. 0);  --  FLAG

   subtype Sub_Element_Array is Element_Array;
   Null_Sub_Element_Array : Sub_Element_Array (1 .. 0);  --  FLAG

end Simple;
