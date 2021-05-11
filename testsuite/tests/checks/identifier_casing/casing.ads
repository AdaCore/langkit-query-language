package Casing is
   type ENUM_1 is (A1, B1, C1);      --  NO FLAG
   type Enum_2 is (A2, B2, C2);      --  FLAG

   Var1 : Enum_1 := A1;              --  NO FLAG
   VAR2 : ENUM_2 := A2;              --  FLAG
end Casing;
