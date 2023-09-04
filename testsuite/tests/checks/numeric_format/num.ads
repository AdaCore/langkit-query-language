package Num is
   A : constant := -1_000_000;             --  NOFLAG
   B : constant := 2#0001_0001_1010_1011#; --  NOFLAG
   C : constant := 8#777_001#;             --  NOFLAG
   D : constant := 16#12AB_C000#;          --  NOFLAG
   E : constant := 3.5E3;                  --  NOFLAG

   F : constant := 1000000;                --  FLAG
   G : constant := 2#0001000110101011#;    --  FLAG
   H : constant := 5#123#;                 --  FLAG
   I : constant := 8#777001#;              --  FLAG
   J : constant := 16#12ab_c000#;          --  FLAG
   K : constant := 3.5e3;                  --  FLAG
end Num;
