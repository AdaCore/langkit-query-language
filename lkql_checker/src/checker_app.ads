with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Helpers; use Libadalang.Helpers;
with GNATCOLL.Opt_Parse;

package Checker_App is

   procedure Job_Post_Process (Context : App_Job_Context);
   --  This procedure will be called once after all units have been parsed.

   package App is new Libadalang.Helpers.App
     (Name             => "lkql-checker",
      Description      => "LKQL based rule checker",
      Job_Post_Process => Job_Post_Process);

   package Args is
      use GNATCOLL.Opt_Parse;

      package Rules is new Parse_Option_List
        (Parser     => App.Args.Parser,
         Short      => "-r",
         Long       => "--rule",
         Help       => "Rule to apply (if not passed, all rules are applied)",
         Accumulate => True,
         Arg_Type   => Unbounded_String);
      --  We use an option rt. a positional arg because we cannot add anymore
      --  positional args to the App parser.

   end Args;

end Checker_App;
