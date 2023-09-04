package body P1 is
   function Read (A : Integer) return Integer with Import;
   function Read_R return Integer renames Read;

   procedure P is
   begin
      Proc (Read (1), Read (2)); --  FLAG
      Proc (Read (1), 1);        --  NOFLAG
      Proc (Read, Read);         --  FLAG
      Proc (Read2, Read2);       --  FLAG
      Proc (Read, Read2);        --  NOFLAG
      Proc (Func1 (Read), Read); --  FLAG
      Proc (Read, Read_R);       --  FLAG
   end P;

end P1;
