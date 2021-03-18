procedure Bar (I : in out Integer) is

begin
   if I = Integer'Last then
      raise Constraint_Error;    --  FLAG
   else
     I := I - 1;
   end if;
exception
   when Constraint_Error =>
      I := Integer'First;
end Bar;
