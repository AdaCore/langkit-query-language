package body P1 is
   function Read (A, B : Integer) return Integer with Import;
   function Read2 (A, B : Integer) return Integer with Import;

   procedure P is
   begin
      Proc (Read, Read);    --  FLAG
      Proc (Read2, Read2);  --  FLAG
      Proc (Read, Read2);   --  NO FLAG
      Proc (Func1 (Read), Read);  --  FLAG
   end P;

end P1;
