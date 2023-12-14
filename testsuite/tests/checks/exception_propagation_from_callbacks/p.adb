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
   end P2;

   procedure Take_Cb (I : Integer; Param : access procedure) is null;

   procedure Calls is
   begin
      Take_Cb (Param => P1'Access, I => 1);   --  FLAG
      Take_Cb (1, P2'Access);                 --  NOFLAG
   end Calls;

   --  Tests on generics

   generic procedure P3_G;

   procedure P3_G is
   begin
      raise Constraint_Error;
   exception
      when others => raise;
   end P3_G;

   procedure P3 is new P3_G;

   generic procedure Take_Cb_G (I : Integer; Param : access procedure);
   generic procedure Take_Cb_G_2 (I : Integer; Param : access procedure);

   generic package Gen_Pkg is
      procedure Take_Cb (I : Integer; Param : access procedure);
   end Gen_Pkg;

   package Pkg_Inst is new Gen_Pkg;

   procedure Take_Cb_I is new Take_Cb_G;
   procedure Take_Cb_I_2 is new Take_Cb_G_2;

   generic package Gen_Pkg_2 is
      generic procedure Gen_Cb (I : Integer; Param : access procedure);
   end Gen_Pkg_2;

   package Pkg_2_Inst is new Gen_Pkg_2;

   procedure Cb_Inst is new Pkg_2_Inst.Gen_Cb;

   procedure Calls2 is
   begin
      Take_Cb (1, P3'Access);           --  FLAG

      --  Check that we can flag a generic subp via its instantiated name
      Take_Cb_I (1, P1'Access);         --  FLAG

      --  Check that we can flag a generic subp via its uninstantiated name
      Take_Cb_I_2 (1, P1'Access);       --  FLAG

      --  Check that we can flag a subp in a generic pkg via its uninstantiated
      --  name
      Pkg_Inst.Take_Cb (1, P1'Access);  --  FLAG

      Cb_Inst (1, P1'Access);           -- FLAG
   end Calls2;

   --  Tests on subunits

   procedure Sep is separate;

   procedure Calls3 is
   begin
      Take_Cb (1, Sep'Access);      --  FLAG
   end Calls3;

end P;
