package Main is
   procedure Test_0;                          --  NOFLAG
   procedure Test_1 (X : out Integer);        --  NOFLAG
   procedure Test_2 (X, Y : out Integer);     --  NOFLAG
   procedure Test_3 (X, Y, Z : out Integer);  --  FLAG
end Main;
