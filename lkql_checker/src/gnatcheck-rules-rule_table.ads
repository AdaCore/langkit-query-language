------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--           G N A T C H E C K . R U L E S . R U L E _ T A B L E            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2023, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines the table for storing rules to be checked by
--  gnatcheck. For any rule the only means to get into this table is the
--  call to the Register_Rule procedure

with GNAT.Table;
with Gnatcheck.Ids; use Gnatcheck.Ids;
with Checker_App;   use Checker_App;

package Gnatcheck.Rules.Rule_Table is

   function No      (Id : Rule_Id) return Boolean;
   function Present (Id : Rule_Id) return Boolean;
   --  Check if the argument represents an existing rule

   procedure Process_Rule_Option
     (Option     : String;
      Defined_At : String);
   --  Processes the rule option taken from the command line or from rule file.
   --
   --  The following options are supported:
   --
   --  +ALL                 - turn all the rules ON
   --  +GLOBAL              - turn all the global rules ON (temporary
   --                         undocumented feature)
   --  -ALL                 - turn all the rules OFF
   --  -GLOBAL              - turn all the global rules OFF (temporary
   --                         undocumented feature)
   --  +R<rule_id>[:params] - turn ON the given rule [for the given parameters
   --                         values]
   --  -R<rule_id>[:params] - turn OFF the given rule [for the given parameters
   --                         values]. At the moment the rule parameter
   --                         for -R option is not implemented, and -R option
   --                         with parameters is just ignored
   --
   --  params is a list of strings separated by a comma. No space character is
   --  allowed next to ':' or to ','.
   --
   --  <rule_id> should be the name (identifier) of a rule that is currently
   --  implemented in gnatcheck, casing is not important
   --
   --  If the actual does not correspond to any of these formats, or if
   --  <rule_id> does not correspond to any of the rules currently implemented
   --  in gnatcheck, the warning message is generated and nothing is changed in
   --  the state of the currently implemented rules
   --
   --  Defined_At should be set to an empty string when processing the rule
   --  option from a command line, or to the short name of the rule file.

   procedure Process_Rule_File (RF_Name : String);
   --  Processes a set of rule options stored in the rule configuration file.
   --  RF_Name is the name of this rule file.
   --
   --  If file with this name does not exist, the corresponding warning message
   --  is generated.
   --
   --  The rule file may contain empty lines, Ada-style comment lines and lines
   --  containing the rule options, Rule options written in the rule file have
   --  the same syntax as rule options given in the command line (see the
   --  documentation for Process_Rule_Option, except that +/-GLOBAL rule
   --  option is not allowed in the rule configuration file), but they can be
   --  written in the free format, that is:
   --
   --  - a rule option can be written in several lines, with the spaces between
   --    the component of the rule options
   --
   --  - end-of-line (Ada-style) comments may be used
   --
   --  Each rule option in the rule file should start from a new line.
   --
   --  For any problem detected when parsing the rule file the corresponding
   --  warning message is generated. Any part of the rule file that can not be
   --  interpreted as an empty line, a comment line or a rule option is
   --  ignored. Any rule option that does not satisfy the requirements stated
   --  in the description of the rule options in documentation of
   --  Process_Rule_Option is also ignored.

   function Processed_Rule_File_Name return String;
   --  Returns the full path to the rule file currently being processed.
   --  Returns an empty string if no rule file is processed at the moment of
   --  the call.

   procedure Rules_Help;
   --  Outputs into Stderr the help info for all the rules currently
   --  implemented in gnatcheck. This procedure does not know about rule
   --  categories and prints out a flat rule list.

   procedure XML_Help;
   --  Prints out the rule help (organized by categories) in XML format for
   --  GNAT Studio needs.

   package All_Rules is new GNAT.Table (
     Table_Component_Type => Rule_Access,
     Table_Index_Type     => Rule_Id,
     Table_Low_Bound      => First_Rule,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Rule table");

   function Get_Rule (Rule_Name : String) return Rule_Id;
   --  Returns the Id for the rule registered under the name Rule_Name or
   --  corresponding to the compiler check denoted by Rule_Name. If there is no
   --  such rule, returns No_Rule. Rule names are not case-sensitive.

   procedure Turn_All_Rules_Off;
   --  Turns OFF all the rules currently implemented in gnatcheck

   procedure Turn_All_Rules_On;
   --  Turns ON all the rules currently implemented in gnatcheck

   procedure Set_Rule_State (For_Rule : Rule_Id; To_State : Rule_States);
   --  Sets the state of the argument rule. Requires Present (For_Rule)

   function Rule_Name (R : Rule_Id) return String;
   --  Returns the name of the rule.

   function Is_Enabled (Rule : Rule_Id) return Boolean;
   --  Checks if the argument is enabled (works for compiler checks as well).
   --  Assumes Present (Rule), otherwise raises Fatal_Error.

   procedure Process_Rules (Ctx : in out Lkql_Context);
   --  Process input rules: Put the rules that have been requested by the user
   --  in internal data structures.

   procedure Process_Requested_Rules (Ctx : in out Lkql_Context);
   --  Process the rules enabled and set Ctx.Traverse_Instantiations
   --  TODO: Useless? See #190

end Gnatcheck.Rules.Rule_Table;
