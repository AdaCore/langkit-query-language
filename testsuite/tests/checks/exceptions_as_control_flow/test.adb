procedure Test is
begin
   raise Constraint_Error;  --  FLAG;
exception
   when Constraint_Error => null;
   when others => raise Constraint_Error;  --  NOFLAG
end Test;
