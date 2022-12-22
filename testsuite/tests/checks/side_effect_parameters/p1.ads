package P1 is
   function Read return Integer with Import;
   function Read2 return Integer with Import;
   procedure Proc (A, B : Integer) is null;
   function Func1 (A : Integer) return Integer is (A);
   procedure P;
end P1;
