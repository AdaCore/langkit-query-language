procedure Main is
   procedure Valid (X : access procedure) is null;
   procedure Invalid (X, Y : access procedure) is null;

   procedure May_Propagate is
   begin
      raise Constraint_Error;
   end May_Propagate;
begin
   Valid (May_Propagate'Access);
   Invalid (X => May_Propagate'Access, Y => May_Propagate'Access);  --  FLAG (2)
end Main;
