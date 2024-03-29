procedure Params is

   procedure Proc1 (I : out Integer;       --  NOFLAG
                    B : out Boolean) is
   begin
     B := True;
     I := 1;
   end;

   type Arr is array (1 .. 10) of Integer;

   type Rec2 is record
      F1 : Integer;
      F2 : Integer;
   end record;

   type Rec is record
      C1 : Arr;
      C2 : Integer;
      C3 : Rec2;
   end record;

   procedure Proc2
     (P1 : out Arr;        --  FLAG with ignore_component_assignments
      P2 : out Rec;        --  FLAG with ignore_component_assignments
      P3 : out Boolean) is
   begin
      P2.C1 (1) := 1;
      P2.C2 := 1;
      P2.C3.F1 := 1;
      Proc1 (P1 (1), P3);
   end;

   procedure Proc3
     (R1 : out Rec2;     --  FLAG with ignore_component_assignments
      R2 : out Rec2)     --  FLAG with ignore_component_assignments
   is
      X : Integer renames R2.F1;
   begin
      R1.F1 := 1;
      X     := 1;
   end;

begin
   null;
end Params;
