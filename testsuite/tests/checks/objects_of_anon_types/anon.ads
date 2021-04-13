package Anon is
   type Arr is array (1 .. 10) of Integer;
   type Acc is access Integer;

   A : array (1 .. 10) of Integer;  --  FLAG
   B : Arr;                         --  NO FLAG

   C : access Integer;              --  FLAG
   D : Acc;                         --  NO FLAG

   generic
      F1 : access Integer;          --  FLAG
      F2 : Acc;                     --  NO FLAG
   procedure Proc_G
     (P1 : access Integer;          --  NO FLAG
      P2 : Acc);                    --  NO FLAG
end Anon;
