package Anon is
   type Arr is array (1 .. 10) of Integer;
   type Acc is access Integer;

   A : array (1 .. 10) of Integer;  --  FLAG
   B : Arr;                         -- NOFLAG

   C : access Integer;              --  FLAG
   D : Acc;                         -- NOFLAG

   generic
      F1 : access Integer;          --  FLAG
      F2 : Acc;                     -- NOFLAG
   procedure Proc_G
     (P1 : access Integer;          -- NOFLAG
      P2 : Acc);                    -- NOFLAG
end Anon;
