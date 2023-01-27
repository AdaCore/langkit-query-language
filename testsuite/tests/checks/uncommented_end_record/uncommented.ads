package Uncommented is
   type R is record
      I : Integer;
   end record;       --  NO FLAG

   type R is record
      I : Integer;
      J : Integer;
   end record;     --  FLAG if N >= 3

   type R2 is record
      I1 : Integer;
      I2 : Integer;
   end record ;    -- R2  NO FLAG

   type R2 is record
      I : Integer;
      J : Integer;
   end record;     --  R2  NO FLAG
end;
