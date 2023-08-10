package Bit is
   type My_Mod is mod 8;

   type Rec1 is record              --  NOFLAG
      I : My_Mod;
   end record;
   for Rec1 use record
      I at 8 range 0 ..63;
   end record;

   type Rec2_PP is record           --  FLAG
      I : My_Mod;
   end record;
   for Rec2_PP'Size use 32;
   pragma Pack (Rec2_PP);

   type Rec2_PA is record           --  FLAG
      I : My_Mod;
   end record with Pack, Size => 32;
end Bit;
