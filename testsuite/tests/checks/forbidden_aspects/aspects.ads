package Aspects is

   type Arr is array (1 .. 8) of Boolean
     with Pack;                   --  FLAG

   I : Integer
     with Atomic,                 --  FLAG
     Volatile;                    -- NOFLAG

   type T is tagged null record;

   procedure Proc1 (X : in out T)
     with Pre => True;            -- NOFLAG

   procedure Proc2 (X : in out T)
     with Pre'Class => True;      --  FLAG

end Aspects;
