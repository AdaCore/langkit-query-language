procedure Overlay (I : Integer) is

   type Rec is record
      C : Integer;
   end record;

   Var_Rec : Rec;
   Var_Int : Integer;

   Var1 : Integer with Address => Var_Int'Address;    -- NOFLAG
   Var2 : Integer with Address => Var_Rec.C'Address;  --  FLAG
   Var3 : Integer;
   for Var3'Address use I'Address;                    -- NOFLAG

begin
   null;
end Overlay;
