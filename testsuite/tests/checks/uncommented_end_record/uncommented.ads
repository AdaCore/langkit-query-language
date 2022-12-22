package Uncommented is
   type R is record  --  NO FLAG
      I : Integer;
   end record;

   type R is record
      I : Integer;
      J : Integer;
   end record;     --  FLAG if N > 3

   type R2 is record  --  NO FLAG
      I : Integer;
      J : Integer;
   end record;     --  R2
end;
