package body Uncommented is
   procedure Proc (I : out Integer) is
   begin
      I := Var;
   end Proc;

   package body Inner is
      procedure Inner_Proc (I : out Integer) is
      begin
         I := Inner_Var;
      end  ;
   begin  -- Inner
      Inner_Var := 1;
   end Inner;
begin                 --  FLAG
   Var := Inner.Inner_Var + 1;
end;
