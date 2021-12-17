procedure For_Loop_Constraint_Error is

   type Id is range 1 .. 100;
   type Float_Array_Type is array (Id range <>) of Float;

   type Float_Adjustable_Array_Type (Size : Id) is record
      A : Float_Array_Type (1 .. Size);
   end record;

   type Object (Size : Id) is record
      Float_List : Float_Adjustable_Array_Type (Size);
   end record;

   procedure Print (Self : in out Object) is
   begin
      for Element of Self.Float_List.A loop   --  FLAG
         null;
      end loop;
   end Print;

   O : Object :=
     (Size       => 2,
      Float_List =>
        (Size => 2,
         A    =>
           (1 =>  1.0,
            2 =>  2.0)));
begin
   for Element of O.Float_List.A loop        --  FLAG
      null;
   end loop;

   Print (O);
end For_Loop_Constraint_Error;
