procedure Pos is

   procedure Proc (I : in out Integer; J : Integer := 0) is
   begin
      I := I + J;
   end Proc;

   Var1, Var2 : Integer;

begin
   Proc (Var1, Var2);   --  FLAG
end Pos;
