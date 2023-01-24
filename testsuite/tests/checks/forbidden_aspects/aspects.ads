package Aspects is

   type Arr is array (1 .. 8) of Boolean
     with Pack;                   --  FLAG

   I : Integer
     with Atomic,                 --  FLAG
     Volatile;                    --  NO FLAG

   type T is tagged null record;

   procedure Proc1 (X : in out T)
     with Pre => True;            --  NO FLAG

   procedure Proc2 (X : in out T)
     with Pre'Class => True;      --  FLAG

end Aspects;
