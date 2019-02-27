with Run;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

----------
-- Main --
----------

procedure Main is
begin
   case Argument_Count is
      when 1 =>
         Run.Run_Standalone_Query (Argument (1));
      when 2 =>
         Run.Run_Against_Project (Argument (1), Argument (2));
      when others =>
         Put_Line ("Expected 1..2 argument !");
   end case;
end Main;
