package Pack is
   type Rec is record
      C1 : Integer;
      C2 : Boolean;
   end record;

   type Int is range 1 .. 10_000;
   for Int'Size use 32;                   -- NOFLAG

   subtype S_Int is Int range 2 .. 2_000;

   type Int_1 is range 1 .. 100_000;
   for Int_1'Size use Int'Size;           -- NOFLAG

   type Int_2 is range 0 .. 7
     with Size => 2 * Int'Size;           -- NOFLAG

   Var_Rec : Rec;
   Var_Int : Int;
end Pack;
