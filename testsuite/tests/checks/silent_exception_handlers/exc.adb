with Ada.Exceptions; use Ada.Exceptions;

procedure Exc is
   procedure Log (Msg : String) with Import;

   I : Integer := 0;
begin
   begin
      I := I + 1;
   exception
      when others =>   --  FLAG
         null;
   end;

exception
   when Constraint_Error =>  --  NOFLAG
      declare
      begin
         raise;
      end;
   when Program_Error =>     --  NOFLAG
      if I = 0 then
         Log ("0");
      elsif I = 1 then
         Log ("1");
      else
         case I is
            when 2 =>
               Log ("2");
            when others =>
               raise
         end case;
      end if;
   when E : others =>        --  NOFLAG
      I := 0;
      Log (Exception_Message (E));
end Exc;
