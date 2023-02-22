separate (P)
procedure Sep is
begin
   raise Constraint_Error;
exception
   when others => raise;
end Sep;
