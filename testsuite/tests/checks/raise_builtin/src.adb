with Ada.Text_IO; use Ada.Text_IO;

procedure Raise_Builtin is
   Ren : exception renames Constraint_Error;
   Ren2 : exception renames Ren;
begin
   raise Program_Error with "Message";  --  FLAG
   raise Funky_Error;                   --  NO FLAG
   raise Ren;                           --  FLAG

exception
   when others =>
      raise Ren2;                       --  FLAG
end Raise_Builtin;
