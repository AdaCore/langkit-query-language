------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Opt_Parse;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers; use Libadalang.Helpers;

with LKQL.Errors;
with Rule_Commands; use Rule_Commands;
with Langkit_Support.Text; use Langkit_Support.Text;

package Checker_App is

   procedure Job_Setup (Context : App_Job_Context);

   procedure Process_Unit
     (Context : App_Job_Context; Unit : Analysis_Unit);
   --  This procedure will be called once after all units have been parsed.

   package App is new Libadalang.Helpers.App
     (Name             => "lkql-checker",
      Description      => "LKQL based rule checker",
      Process_Unit     => Process_Unit,
      Job_Setup        => Job_Setup);

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
