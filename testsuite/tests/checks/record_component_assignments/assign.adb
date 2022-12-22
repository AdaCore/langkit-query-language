procedure Assign is
   type Rec1 is record
      Comp1, Comp2 : Integer;
   end record;

   type Rec2 is record
      Comp1 : Integer;
   end record;

   O1 : Rec1;
   O2 : Rec2;

begin
   O1.Comp1 := 1;   --  FLAG
   O2.Comp1 := 1;   --  NO FLAG (single component)

   O1.Comp2 := 1;   --  NO FLAG (flag only first assignment)
   O1.Comp1 := 0;   --  NO FLAG (flag only first assignment)
end Assign;
