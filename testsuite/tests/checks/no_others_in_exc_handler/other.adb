procedure Other (I, J : in out Integer) is
begin
   begin
      I := I + 1;
   exception                --  FLAG
      when Constraint_Error => null;
   end;

exception                    --  NO FLAG
   when Constraint_Error =>
      I := Integer'Last;
   when others =>
      I := J;
      raise;
end Other;
