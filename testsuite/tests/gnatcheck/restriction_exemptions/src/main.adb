package body Main is

   --------------------
   -- Protected_Type --
   --------------------
   pragma Annotate (GNATcheck, Exempt_On, "Restrictions: No_Access_Subprograms", "testing");
   protected body Protected_Type is
      entry Hello when True is
      begin
         null;
      end Hello;
   end Protected_Type;
   pragma Annotate (GNATcheck, Exempt_Off, "Restrictions: No_Access_Subprograms");  --  NOFLAG because "No_Access_Subprograms" is not active

   pragma Annotate (GNATcheck, Exempt_On, "Restrictions: No_Access_Subprograms, Max_Protected_Entries", "testing");
   procedure Test is
   begin
      null;
   end Test;
   pragma Annotate (GNATcheck, Exempt_Off, "Restrictions: No_Access_Subprograms, Max_Protected_Entries");  --  FLAG because "Max_Protected_Entries" is active

end Main;
