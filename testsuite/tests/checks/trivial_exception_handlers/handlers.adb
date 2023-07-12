procedure Handlers is
begin
   begin
      null;
   exception
      when Constraint_Error =>  -- NOFLAG
         raise;
      when others =>
         null;
   end;

   begin
      null;
   exception
      when Constraint_Error =>  --  FLAG
         raise;
      when others =>   --  FLAG
         raise;
   end;
exception
   when Constraint_Error =>  --  FLAG
      raise;
   when Program_Error =>  --  FLAG
      raise;
end Handlers;
