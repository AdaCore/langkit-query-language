package body P1 is
   function Read (A, B : Integer) return Integer with Import;
   function Read2 (A, B : Integer) return Integer with Import;
   function Read_R return Integer renames Read;

   procedure P is
   begin
      Proc (Read, Read);    --  FLAG
      Proc (Read2, Read2);  --  FLAG
      Proc (Read, Read2);   --  NO FLAG
      Proc (Func1 (Read), Read);  --  FLAG
      Proc (Read, Read_R);  --  FLAG
   end P;

end P1;
