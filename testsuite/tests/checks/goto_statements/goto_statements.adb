package body Goto_Statements is


   procedure P is
   begin
      goto A;  --  FLAG
      <<A>>
   end P;

end Goto_Statements;
