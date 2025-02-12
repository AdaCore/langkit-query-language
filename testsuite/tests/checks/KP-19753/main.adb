procedure Main is
   A : array (1 .. 5) of Integer;
begin
   for I in 1 .. 5 loop          --  FLAG
      raise Constraint_Error;
   end loop;

   for I of A loop               --  FLAG
      raise Constraint_Error;
   end loop;

   loop                          --  FLAG
      raise Constraint_Error;
   end loop;

   loop                          --  FLAG
      null;
      null;
      null;
      raise Constraint_Error;
   end loop;

   loop                          --  FLAG
      null;
      null;
      null;
      raise Constraint_Error;
      A (1) := 0;
   end loop;

   loop                          --  NOFLAG
      A (1) := 0;
      raise Constraint_Error;
   end loop;
end Main;
