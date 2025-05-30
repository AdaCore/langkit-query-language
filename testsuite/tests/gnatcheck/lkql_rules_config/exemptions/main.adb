with Ada.Text_IO;  --  FLAG

pragma Annotate (gnatcheck, Exempt_On, "Warnings", "Exempting unused unbounded");
with Ada.Strings.Unbounded;  --  NOFLAG (exempted)
pragma Annotate (gnatcheck, Exempt_Off, "Warnings");

with Ada.Containers;  --  FLAG

procedure Main is
begin
   goto x;  --  FLAG
   <<x>>

   --## rule off Goto_Statements ## Exempting gotos
   goto y;  --  NOFLAG (exempted)
   <<y>>
   --## rule on Goto_Statements

   goto z;  --  FLAG
   <<z>>
end Main;
