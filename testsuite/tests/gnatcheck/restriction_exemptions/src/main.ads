package Main is
   --  Test the restriction exemption with a numeric parameter
   pragma Annotate (GNATcheck, Exempt_On, "Restrictions: No_Protected_Types, Max_Protected_Entries", "testing");
   protected type Protected_Type is
      entry Hello;  --  FLAG
   pragma Annotate (GNATcheck, Exempt_Off, "Restrictions: No_Protected_Types, Max_Protected_Entries");
   end Protected_Type;
end Main;
