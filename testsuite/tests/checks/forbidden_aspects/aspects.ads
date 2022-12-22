package Aspects is

   type Arr is array (1 .. 8) of Boolean
     with Pack;                   --  FLAG

   I : Integer
     with Atomic,                 --  FLAG
     Volatile;                    --  NO FLAG

end Aspects;
