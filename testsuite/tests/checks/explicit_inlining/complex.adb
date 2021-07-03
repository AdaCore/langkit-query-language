package body Complex is

   procedure Swap (I, J : in out Integer) is
   begin
      null;
   end Swap;

   procedure With_UC with Inline is   --  FLAG
   begin
      null;
   end With_UC;

   generic
   procedure Gen_Proc;

   procedure Gen_Proc is
   begin
      null;
   end Gen_Proc;

   procedure Inst_Proc is new Gen_Proc with Inline;   --  FLAG

end Complex;
