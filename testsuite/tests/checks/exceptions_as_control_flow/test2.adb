procedure Test2 is
begin
   begin
      raise Constraint_Error;  --  FLAG
   exception
      when Constraint_Error => null;
   end;

   raise Constraint_Error; -- NO FLAG
end Test2;

