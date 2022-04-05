procedure Main is
   type Enum is (A, B, C);
   for Enum use (A => -1, B => 0, C => 2);

   type Enum2 is (E, F, G);

   type Enum3 is (H, I, J);
   for Enum3 use (H => 1, I => 2, J => 4);

   Var : Enum;
   X   : Integer;
begin
   X := Enum'Enum_Rep (Var);   --  FLAG
   X := Enum2'Enum_Rep (E);    --  NO FLAG
   X := Enum3'Enum_Rep (H);    --  NO FLAG
end Main;
