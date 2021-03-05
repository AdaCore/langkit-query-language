package body Binary_Case_Statements is

   procedure P is
      Var : Integer := 0;
   begin
      case Var is                   --  FLAG
      when 1 =>
         Var := Var + 1;
      when others =>
         null;
      end case;
   end P;

end Binary_Case_Statements;
