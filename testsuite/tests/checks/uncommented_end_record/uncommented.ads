package Uncommented is
   type R is record
      I : Integer;
   end record;       --  NOFLAG

   type R is record
      I : Integer;
      J : Integer;
   end record;     --  FLAG if N >= 3

   type R2 is record
      I1 : Integer;
      I2 : Integer;
   end record ;    -- R2  NOFLAG

   type R3 is record
      I : Integer;
      J : Integer;
   end record;     --  R3  NOFLAG

   type R4 is record
      I : Integer;
      J : Integer;
   end record;     --  FLAG
   --  R4
end;
