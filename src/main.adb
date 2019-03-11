with Run;

with Libadalang.Analysis;      use Libadalang.Analysis;
with Libadalang.Auto_Provider; use Libadalang.Auto_Provider;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.VFS;       use GNATCOLL.VFS;
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

      package Project_Path is new Parse_Option
        (Parser      => Parser,
         Long        => "--project",
         Short       => "-p",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Path of the GPR project file");

      package Auto_Dirs is new Parse_Option_List
        (Parser => Parser,
         Short  => "-a",
         Long   => "--auto_dirs",
         Arg_Type => Unbounded_String,
         Accumulate => True,
         Help       =>
            "Directories to use for the auto provider. If one is passed, "
            & "auto provider will be used, and project options ignored");

      package Recovery_Enabled is new Parse_Flag
        (Parser => Parser,
         Short  => "-r",
         Long   => "--recover",
         Help   => "Enable error recovery");

   end Arg;

begin
   if not Arg.Parser.Parse then
      return;
   end if;

   if Arg.Auto_Dirs.Get'Length > 0 then
      declare
         Auto_Dirs : Arg.Auto_Dirs.Result_Array renames Arg.Auto_Dirs.Get;
         Dirs      : GNATCOLL.VFS.File_Array (Auto_Dirs'Range);
         Files     : GNATCOLL.VFS.File_Array_Access;
         UFP       : Unit_Provider_Reference;
         Context   : Analysis_Context;
      begin
         for I in Dirs'Range loop
            Dirs (I) := Create (+To_String (Auto_Dirs (I)));
         end loop;

         Files := Find_Files (Directories => Dirs);
         UFP := Create_Auto_Provider_Reference (Files.all);
         Context := Create_Context (Unit_Provider => UFP);

         Run.Run_On_Files (To_String (Arg.Script_Path.Get),
                           Context,
                           Files,
                           Arg.Recovery_Enabled.Get);

         GNATCOLL.VFS.Unchecked_Free (Files);
      end;

   elsif Length (Arg.Project_Path.Get) > 0 then
      Run.Run_Against_Project (To_String (Arg.Script_Path.Get),
                               To_String (Arg.Project_Path.Get),
                               Arg.Recovery_Enabled.Get);
   end if;
end Main;
