with Ada.Exceptions; use Ada.Exceptions;

procedure Other (I, J : in out Integer) is
   procedure Log (Msg : String) with Import;
begin
   begin
      I := I + 1;
   exception
      when others =>   --  FLAG
         null;
   end;

exception
   when Constraint_Error =>
      I := Integer'Last;
   when E : others =>      --  NO FLAG
      I := J;
      Log (Exception_Message (E));
end Other;
