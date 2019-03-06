with Run;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

--  Usage:
--  main [SCRIPT_PATH] [PROJECT_PATH] [OPTIONS]
--
--  * SCRIPT_PATH:  path of the LKQL script to evaluate
--  * PROJECT_PATH: path of a GPR project file. The script will be evaluated
--                  on all source file that belong to the project.
--  * OPTIONS : -r enable error recovery

----------
-- Main --
----------

procedure Main is

   package Arg is

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "The script will be evaluated on all source files that " &
                 "belong to the project.");

      package Script_Path is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "script_path",
         Arg_Type => Unbounded_String,
         Help     => "Path of the LKQL script to evaluate");

      package Project_Path is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "project_path",
         Arg_Type => Unbounded_String,
         Help     => "Path of the GPR project file");

      package Recovery_Enabled is new Parse_Flag
        (Parser => Parser,
         Short  => "-r",
         Long   => "--recover",
         Help   => "Enable error recovery");

   end Arg;

begin
   if Arg.Parser.Parse then
      Run.Run_Against_Project (To_String (Arg.Script_Path.Get),
                               To_String (Arg.Project_Path.Get),
                               Arg.Recovery_Enabled.Get);
   end if;
end Main;
