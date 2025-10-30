package Main is
   type Pub_Range is range 1 .. 2 with Size => 8;  --  NOFLAG
   type Priv_Range is private;                     --  FLAG
   subtype Priv_Range_Sub is Priv_Range;           --  NOFLAG
   type Priv_Float_Range is private;               --  FLAG
   type Priv_Enum is private;                      --  FLAG
   type Priv_Range_No_Size is private;             --  NOFLAG
   type Priv_Arr is private;                       --  NOFLAG
private
   type Priv_Range is range 1 .. 2 with Size => 8;
   type Priv_Float_Range is digits 8 with Size => 64;
   type Priv_Enum is (A, B, C) with Size => 8;
   type Priv_Range_No_Size is range 1 .. 2;
   type Priv_Arr is array (1 .. 2) of Integer with Size => 64;
end Main;
