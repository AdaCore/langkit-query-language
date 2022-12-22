package body Params is
   procedure Proc (P1 : Rec; P2 : out Rec) is null;
   procedure Proc2 (P1 : out Integer; P2 : out Rec) is null;
   procedure Proc3 (P1 : in out Rec; P2 : out Rec) is null;
   procedure Proc4 (P1 : Integer; P2 : out Integer) is null;
   procedure Proc5 (P1 : out Integer; P2 : out Integer) is null;
   procedure Proc6 (P1 : out Rec2; P2 : out Integer) is null;

   procedure Test is
      A, B : Rec;
      C    : Rec2;
      Arr  : array (1 .. 10) of Integer;
      I, J : Integer;

   begin
      Proc (A, A);                 --  FLAG if with_in
      Proc3 (Test.A, A);           --  FLAG
      Proc2 (Test.A.Field1, A);    --  FLAG
      Proc3 (A, A);                --  FLAG
      Proc3 (A, B);                --  NO FLAG
      Proc4 (A.Field1, B.Field1);  --  NO FLAG  (by-copy)
      Proc5 (Arr (I), Arr (J));    --  NO FLAG
      Proc5 (Arr (1), Arr (2));    --  NO FLAG
      Proc6 (C, C.F.Field1);       --  FLAG
   end Test;

end Params;
