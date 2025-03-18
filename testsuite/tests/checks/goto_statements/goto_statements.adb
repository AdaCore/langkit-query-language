package body Goto_Statements is

   function Foo return Boolean is (True);

   procedure P is
   begin
      goto A;  --  FLAG
      <<A>>

      if Foo then
         goto B;  --  NOFLAG
      end if;

      <<B>>
   end P;

end Goto_Statements;
