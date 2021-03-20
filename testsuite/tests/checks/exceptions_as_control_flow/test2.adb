procedure Test2 is
   CE  : exception renames Constraint_Error;
   CE2 : exception renames CE;
begin
   begin
      raise Constraint_Error;  --  FLAG
   exception
      when CE => null;
   end;

   begin
      raise CE2;  --  FLAG
   exception
      when Constraint_Error => null;
   end;

   raise Constraint_Error; -- NO FLAG
end Test2;

