package Num is

   I : Integer;                               -- FLAG
   F : Float;                                 -- FLAG
   B : Boolean;                               -- NOFLAG

   type Arr is array (1 .. 5) of Short_Float; -- FLAG

   type Res is record
      C1 : Long_Integer;                      -- FLAG
      C2 : Character;                         -- NOFLAG
   end record;

   Duration_Machine_Radix : Integer := Duration'Machine_Radix; -- FLAG (2)
end Num;
