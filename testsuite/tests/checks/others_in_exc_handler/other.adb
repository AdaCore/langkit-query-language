procedure Other (I, J : in out Integer) is
begin
    I := I + 1;
exception
   when Constraint_Error =>
      I := Integer'Last;
   when others =>                   --  FLAG
      I := J;
      raise;
end Other;
