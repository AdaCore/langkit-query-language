package Explicit_Full_Discrete_Ranges is
   type T1 is new Float range 1.0 .. 2.0;

   type T2 is delta 0.1 range T1'First .. T1'Last;           -- NOFLAG
   type T3 is delta 0.1 digits 7 range T1'First .. T1'Last;  -- NOFLAG

   procedure P;
end Explicit_Full_Discrete_Ranges;
