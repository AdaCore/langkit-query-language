procedure P is
   type Matrix is array (Integer range 1 .. 10, Integer range 20 ..30) of Boolean;
   Columns : constant Integer := 2;
   First_Column : Integer := Matrix'First(Columns);  --  FLAG
begin
   pragma Assert (First_Column = 20);
   pragma Assert (Matrix'Last (Columns) = 30);    --  FLAG
   pragma Assert (Matrix'Length (Columns) = 11);  --  FLAG

   for J in Matrix'Range (Columns) loop   --  FLAG
      pragma Assert (J = 20);
      exit;
   end loop;
end;
