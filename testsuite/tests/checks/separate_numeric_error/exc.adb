procedure Exc is
begin
   begin
      null;
   exception
      when Numeric_Error =>   --  FLAG
         null;
   end;

   begin
      null;
   exception
      when Constraint_Error | Numeric_Error =>   --  NO FLAG
         null;
   end;

exception
   when Constraint_Error =>   --  FLAG
      null;
end;
