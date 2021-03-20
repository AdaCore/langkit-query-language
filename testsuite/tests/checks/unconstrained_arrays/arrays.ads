package Arrays is

   type Idx is range -100 .. 100;

   type U_Arr is array (Idx range <>) of Integer;   --  FLAG
   type C_Arr is array (Idx) of Integer;            --  NO FLAG

end Arrays;
