package Pragmas is

   type Arr is array (1 .. 8) of Boolean;
   pragma Pack (Arr);             --  FLAG

   I : Integer;
   pragma Atomic (I);             --  FLAG
   pragma Volatile (I);           -- NOFLAG

end Pragmas;
