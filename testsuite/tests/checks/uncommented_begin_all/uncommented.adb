package body Uncommented is
   procedure Proc (I : out Integer) is
      J : Integer;
   begin                --  FLAG
      I := Var;
   end Proc;

   procedure Proc_2 is
      I : Integer;
      S : String := "Hello";
   begin  -- Proc_2
      --  A comment
      null;
   end Name;

   procedure Proc_3 is
   begin                --  NOFLAG
      null;
   end Proc_3;

begin                   --  FLAG
   Var := Inner.Inner_Var + 1;
end;
