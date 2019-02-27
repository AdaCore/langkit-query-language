with Run;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

--  The program can be launched wth either 1 or 2 command-line arguments.
--  The first argument is the path of the LKQL script to be run.
--  The optionnal second argument is the path of a GPR project file.
--
--  * When launched with one argument, the program will run the script in
--    standalone mode, and raise an Eval_Error on every query.
--  * When launched with a second argument, the script will executed on every
--    source file of the project.

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
