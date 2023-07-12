package Casing is
   type ENUM_1 is (A1, B1, C1);      -- NOFLAG
   type Enum_2 is (A2, B2, C2);      --  FLAG

   Var1 : Enum_1 := A1;              -- NOFLAG
   VAR2 : ENUM_2 := A2;              --  FLAG
end Casing;
