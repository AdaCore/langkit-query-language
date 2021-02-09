with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Libadalang.Helpers; use Libadalang.Helpers;
with GNATCOLL.Opt_Parse;

with LKQL.Errors;
with Rule_Commands; use Rule_Commands;
with Langkit_Support.Text; use Langkit_Support.Text;

package Checker_App is

   procedure Job_Post_Process (Context : App_Job_Context);
   --  This procedure will be called once after all units have been parsed.

   package App is new Libadalang.Helpers.App
     (Name             => "lkql-checker",
      Description      => "LKQL based rule checker",
      Job_Post_Process => Job_Post_Process);

   package Args is
      use GNATCOLL.Opt_Parse;

      type Qualified_Rule_Argument is record
         Rule_Name : Unbounded_Text_Type;
         Arg       : Rule_Argument;
      end record;
      --  Argument for a rule, including the rule name. Directly parsed from
      --  the command line.

      function Convert (Raw_Arg : String) return Qualified_Rule_Argument;
      --  Convert a string of the form "rule_name.arg_name=val" into a
      --  ``Qualified_Rule_Argument``.

      package Rules is new Parse_Option_List
        (Parser     => App.Args.Parser,
         Short      => "-r",
         Long       => "--rule",
         Help       => "Rule to apply (if not passed, all rules are applied)",
         Accumulate => True,
         Arg_Type   => Unbounded_String);
      --  We use an option rt. a positional arg because we cannot add anymore
      --  positional args to the App parser.

      package Rules_Args is new Parse_Option_List
        (Parser     => App.Args.Parser,
         Short      => "-a",
         Long       => "--rule-arg",
         Help       => "Argument to pass to a rule, with the syntax "
         & "<rule_name>.<arg_name> = <arg_value>",
         Accumulate => True,
         Arg_Type   => Qualified_Rule_Argument);
      --  We use an option rt. a positional arg because we cannot add anymore
      --  positional args to the App parser.

      package Property_Error_Recovery is new Parse_Enum_Option
        (Parser => App.Args.Parser,
         Short  => "-pr",
         Long   => "--property-error-recovery",
         Help   =>
            "Which behavior to adopt when there is a property error"
            & "inside of a LKQL query",
         Arg_Type => LKQL.Errors.Property_Error_Recovery_Kind,
         Default_Val => LKQL.Errors.Continue_And_Warn);
   end Args;

end Checker_App;
