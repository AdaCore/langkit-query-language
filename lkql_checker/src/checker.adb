with Diagnostics;   use Diagnostics;
with Rules_Factory; use Rules_Factory;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Checker is

   package Args is

      Parser : Argument_Parser := Create_Argument_Parser
        ("All the checks will be run on the project's files");

      package Project_Path is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "project path",
         Help     => "Path of the Ada project",
         Arg_Type => Unbounded_String);

   end Args;

   D : Diagnostic;
begin
   if not Args.Parser.Parse then
      return;
   end if;

   D := Make_Diagnostic_For_Project
     (To_String (Args.Project_Path.Get), All_Rules);

   for R of D.Evaluate loop
      R.Display;
   end loop;
end Checker;
