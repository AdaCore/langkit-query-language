package Suffix is
   type Int   is range 0 .. 100;      --  FLAG
   type Int_T is range 0 .. 100;      --  NO FLAG

   type Int_A   is access Int;        --  FLAG
   type Int_PTR is access Int;        --  NO FLAG

   Const   : constant Int := 1;       --  FLAG
   Const_C : constant Int := 1;       --  NO FLAG

end Suffix;
