procedure Overlay (I : Integer) is

   C : constant Integer := 1;
   V : Integer;
   for V'Address use C'Address;           --  FLAG

   W : Integer with Address => C'Address; --  FLAG

   C1 : constant Integer := 1;
   V1 : constant Integer with Import;
   for V1'Address use C1'Address;         --  NO FLAG

   C2 : constant Integer := 1;
   V2 : constant Integer with Import, Volatile;
   for V2'Address use C2'Address;         --  FLAG

   generic
      Var : in Integer;
   package Pack is
      Const_3 : Integer with
        Address => Var'Address;           --  FLAG
   end Pack;

   Var_Int_1 : Integer;
   for Var_Int_1'Address use I'Address;   --  FLAG

   Object : constant Integer := 0;
   Ren1   : Integer renames Object;
   Ren2   : Integer renames Ren1;
   V3     : Integer with Address => Ren2'Address;   --  FLAG

begin
   null;
end Overlay;
