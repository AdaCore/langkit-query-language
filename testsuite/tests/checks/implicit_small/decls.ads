package Decls is
   type Fraction is delta 0.01 range -1.0 .. 1.0;
   type Fraction1 is delta 0.01 range -1.0 .. 1.0; --  FLAG

   type Money is delta 0.01 digits 15;

   for Fraction'Small use 0.01;
end Decls;
