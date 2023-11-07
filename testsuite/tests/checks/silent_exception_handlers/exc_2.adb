with Ada.Exceptions; use Ada.Exceptions;

procedure Exc_2 is
   procedure Log (Msg : String) with Import;

   I : Integer := 0;
begin
   null;

exception
   when Constraint_Error =>  --  FLAG
      if I = 1 then
         raise;
      elsif I = 2 then
         Log ("");
      elsif I = 3 then
         null;
      else
         Log ("");
      end if;
   when Program_Error =>     --  FLAG
      while I < 1 loop
         I := I + 1;
         Log ("");
      end loop;
   when E : others =>        --  FLAG
      for X in 1 .. I loop
         I := 0;
         Log (Exception_Message (E));
      end loop;
end Exc_2;
