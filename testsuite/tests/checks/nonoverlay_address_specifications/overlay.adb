procedure Overlay (I : Integer) is

   type Rec is record
      C : Integer;
   end record;

   Var_Rec : Rec;
   Var_Int : Integer;

   Var1 : Integer with Address => Var_Int'Address;    --  NO FLAG
   Var2 : Integer with Address => Var_Rec.C'Address;  --  FLAG
   Var3 : Integer;
   for Var3'Address use I'Address;                    --  NO FLAG

begin
   null;
end Overlay;
