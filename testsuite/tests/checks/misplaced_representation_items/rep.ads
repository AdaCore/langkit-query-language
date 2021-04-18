package Rep is

   type Int1 is range 0 .. 1024;
   type Int2 is range 0 .. 1024;

   for Int2'Size use 16;         --  NO FLAG
   for Int1'Size use 16;         --  FLAG

   type Rec1 is record
      C : Int1;
   end record;

   type Rec2 is record
      C : Int2;
   end record;

   for Rec1 use record           --  FLAG
      C at 0 range  0 .. 15;
   end record;

   Var1 : Integer;
   Var2 : Integer;

   pragma Atomic (Var2);         --  NO FLAG
   pragma Atomic (Var1);         --  FLAG

   type Rec3 is record
      C1 : Integer;
      C2 : Integer;
      pragma Independent (C1);   --  FLAG
   end record;

end Rep;
