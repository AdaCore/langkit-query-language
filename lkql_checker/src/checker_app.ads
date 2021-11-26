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

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;
with Libadalang.Helpers; use Libadalang.Helpers;

with LKQL.Errors;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with Rule_Commands; use Rule_Commands;
with Rules_Factory; use Rules_Factory;

with Langkit_Support.Text; use Langkit_Support.Text;

package Checker_App is

   Exit_App : exception;
   --  Exception raised by the app if it wants to exit with an error status and
   --  a message.

   type Rules_By_Kind is array (Ada_Node_Kind_Type) of Rule_Vector;

   type LKQL_Context is record
      Analysis_Ctx : Analysis_Context;
      Eval_Ctx     : Eval_Context;
      Cached_Rules : Rules_By_Kind := [others => Rule_Vectors.Empty_Vector];
      --  Data structure mapping node kinds to the checks that should be ran
      --  when this node type is encountered.

      All_Rules : Rule_Vector;
      --  All known rules

      Traverse_Instantiations : Boolean := False;
      --  Whether we should traverse generic instantiations. This will be set
      --  to true if there is at least one rule in the active set of rules that
      --  requires it. This is used so we don't traverse generic instantiations
      --  if no rule requires it.
   end record;
   --  Context giving access to all the "global" data structures for an LKQL
   --  analysis.

   type LKQL_Context_Access is access all LKQL_Context;
   --  Access to an LKQL context

   procedure App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array);
   procedure Job_Setup (Context : App_Job_Context);

   type Message_Kinds is (Rule_Violation, Internal_Error);

   procedure Process_Unit
     (Ctx                     : LKQL_Context;
      Unit                    : Analysis_Unit;
      Emit_Message            :
        access procedure (Message    : Unbounded_Text_Type;
                          Unit       : Analysis_Unit;
                          Rule       : Unbounded_Text_Type;
                          Kind       : Message_Kinds;
                          Sloc_Range : Source_Location_Range) := null);
   --  Process one analysis unit.
   --  Call Emit_Message on each match, if non null.

   procedure Process_Unit
     (Context : App_Job_Context; Unit : Analysis_Unit);
   --  Process one analysis unit in a given context

   procedure Job_Post_Process
     (Context : App_Job_Context);

   package App is new Libadalang.Helpers.App
     (Name               => "lkql-checker",
      Description        => "LKQL based rule checker",
      Process_Unit       => Process_Unit,
      App_Setup          => App_Setup,
      Job_Setup          => Job_Setup,
      Enable_Parallelism => True,
      Job_Post_Process   => Job_Post_Process);

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

      package Rules_Dirs is new Parse_Option_List
        (Parser     => App.Args.Parser,
         Long       => "--add-rules-dir",
         Help       => "Add a directory in which rules will be searched",
         Accumulate => True,
         Arg_Type   => Unbounded_String);

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

      package Output_Style is new Parse_Enum_Option
        (Parser      => App.Args.Parser,
         Long        => "--output-style",
         Help        => "Output style for the diagnostic messages",
         Arg_Type    => Output_Style,
         Default_Val => Default);
   end Args;

end Checker_App;
