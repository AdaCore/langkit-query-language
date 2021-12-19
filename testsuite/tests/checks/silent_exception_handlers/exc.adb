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
   when Constraint_Error =>  --  NO FLAG
      raise;
   when Program_Error =>     --  NO FLAG
      Log ("");
   when E : others =>        --  NO FLAG
      I := 0;
      Log (Exception_Message (E));
end Exc;
