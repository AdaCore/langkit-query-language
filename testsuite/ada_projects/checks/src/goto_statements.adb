package body Goto_Statements is

   
   procedure P is
   begin
      goto A;
      <<A>>
   end P;

end Goto_Statements;
