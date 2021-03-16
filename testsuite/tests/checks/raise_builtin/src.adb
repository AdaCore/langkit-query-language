with Ada.Text_IO; use Ada.Text_IO;

procedure Raise_Builtin is
begin
   raise Program_Error with "Message";
   raise Funky_Error;
end Raise_Builtin;