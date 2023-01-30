procedure Assign is
   type Rec1 is record
      Comp1, Comp2 : Integer;
   end record;

   type Rec2 is record
      Comp1 : Integer;
   end record;

   O1, O3 : Rec1;
   O2 : Rec2;

begin
   null;
   O1.Comp1 := 1;   --  FLAG
   O1.Comp2 := 1;   --  NO FLAG (flag only first assignment)

   O2.Comp1 := 1;   --  NO FLAG (single component)

   begin
      O1.Comp1 := 1;   --  NO FLAG (not consecutive assignments on O1)
      O2.Comp2 := 1;
      O1.Comp2 := 1;
   end;

   begin
      null;
      O3.Comp1 := 1;   --  FLAG
      O3.Comp2 := 1;   --  NO FLAG
   end;

   begin
      O3.Comp1 := 1;   --  NO FLAG

      if True then
         null;
      end if;

      O3.Comp2 := 1;   --  NO FLAG
   end;

   declare
      type Rec_Discr (D : Integer) is record
         A, B : Integer;
      end record;

      type Rec_Tagged is tagged record
         A, B : Integer;
      end record;

      Discr : Rec_Discr (1);
      Obj   : Rec_Tagged;
   begin
      Discr.A := 1;   --  NO FLAG (discriminants)
      Discr.B := 1;   --  NO FLAG

      Obj.A := 1;   --  NO FLAG (tagged)
      Obj.B := 1;   --  NO FLAG
   end

   declare
      type List is record
         Prev, Next : Integer;
      end record;

      type Rec is record
         L : List;
      end record;

      Obj1, Obj2 : Rec;

   begin
      Obj1.L.Next := 0;   --  NO FLAG
      Obj2.L.Prev := 0;   --  NO FLAG
   end;
end Assign;
