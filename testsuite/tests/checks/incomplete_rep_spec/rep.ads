package Rep is
   type Rec is record  --  FLAG
      I : Integer;
      B : Boolean;
   end record;

   for Rec use record
      I at 0 range 0 ..31;
      B at 4 range 0 .. 7;
   end record;

   type Rec2 is record  --  NO FLAG
      I : Integer;
   end record;
   pragma Pack (Rec2);
   for Rec2'Size use 32;

   for Rec2 use record
      I at 0 range 0 ..31;
   end record;
end Rep;
