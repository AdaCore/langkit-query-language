package body Uncommented is
   procedure Proc (I : out Integer) is
      J : Integer;
   begin    --  FLAG
      I := Var;
   end Proc;

begin       --  FLAG
   Var := Inner.Inner_Var + 1;
end;
