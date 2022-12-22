package body P is

   procedure P1 is
   begin
      raise Constraint_Error;
   exception
      when others => raise;
   end P1;

   procedure P2 is
   begin
      raise Constraint_Error;
   exception
      when Constraint_Error => null;
      when others => null;
   end P1;

   procedure Take_Cb (I : Integer; Param : access procedure) is null;

   procedure Calls is
   begin
      Take_Cb (Param => P1'Access, I => 1);   --  FLAG
      Take_Cb (1, P2'Access);                 --  NO FLAG
   end Calls;

end P;
