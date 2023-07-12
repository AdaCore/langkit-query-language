procedure P is
   type Matrix is array (1 .. 10, 1 .. 10) of Integer;

   A     : Matrix;
   K     : Matrix;
   Map   : array (0 .. 99) of Integer := (others => 0);
   Key   : Integer := 0;
   Value : Integer;
   C     : constant Integer := 1;

begin
   for I in A'Range (1) loop        -- NOFLAG
      for J in A'Range (2) loop     -- NOFLAG
         A (I, J) := I * 10 + J;
         K (I, J) := Key;
         Map (C)  := Key;
         key := Key + 1;
      end loop;
   end loop;

   for I in A'Range (1) loop        -- NOFLAG
      for J in A'Range (2) loop     --  FLAG
         Map (K (I, J)) := I;
      end loop;
   end loop;
end P;
