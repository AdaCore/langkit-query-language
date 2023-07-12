package Suffix is
   type Int   is range 0 .. 100;      --  FLAG
   type Int_T is range 0 .. 100;      -- NOFLAG

   type Int_A   is access Int;        --  FLAG
   type Int_PTR is access Int;        -- NOFLAG

   Const   : constant Int := 1;       --  FLAG
   Const_C : constant Int := 1;       -- NOFLAG

end Suffix;
