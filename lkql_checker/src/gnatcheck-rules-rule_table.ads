--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines the table for storing rules to be checked by
--  gnatcheck. For any rule the only means to get into this table is the
--  call to the Register_Rule procedure

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with Checker_App; use Checker_App;

package Gnatcheck.Rules.Rule_Table is

   function Present (Rule : Rule_Id) return Boolean;
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

   procedure Process_LKQL_Rule_File (LKQL_RF_Name : String);
   --  Process the given LKQL file as a rule configuration file and populate
   --  the global rule table with its content.
   --
   --  This procedure calls the GNATcheck worker with the `--parse-lkql-config`
   --  option to extract all information from the LKQL file.

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

   package Rule_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Rule_Id,
      Element_Type    => Rule_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");
   All_Rules : Rule_Map.Map;
   --  This map is used to store all parsed rules associated to their
   --  identifiers.

   package Rule_Instance_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Rule_Instance_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   All_Rule_Instances : Rule_Instance_Map.Map;
   --  This global map contains all created instances, from their normalized
   --  name to their address.

   function Get_Rule (Rule_Name : String) return Rule_Id;
   --  Returns the identifier for the provided rule name. This identifier is
   --  associated to the rule information access in `All_Rules` map. This
   --  function also looks into the `All_Rule_Instances` map for possible
   --  aliases of the rule.
   --  If there is no rule designated by `Rule_Name`, this function returns the
   --  `No_Rule_Id` value.

   function Get_Instance
     (Instance_Name : String) return Rule_Instance_Access;
   --  Return an access to the rule instance associated with the given instance
   --  name. If there is no such instance, this function returns null.

   procedure Turn_Instance_On (Instance : Rule_Instance_Access);
   --  Insert the given rule instance in `All_Rule_Instances` to represents its
   --  enabling.

   procedure Turn_Instance_Off (Instance_Name : String);
   --  Remove the instance associated with `Instance_Name` after its
   --  normalization. If there is no instance assocaited to the provided name
   --  this function does nothing.

   procedure Turn_All_Rules_Off;
   --  Turns OFF all the rules currently implemented in gnatcheck

   procedure Turn_All_Rules_On;
   --  Turns ON all the rules currently implemented in gnatcheck. This function
   --  will only turn on rules that are not enabled yet.

   function Rule_Name (Rule : Rule_Id) return String;
   --  Returns the name of the rule.

   function Is_Enabled (Rule : Rule_Id) return Boolean;
   --  Checks if the argument is enabled (works for compiler checks as well).
   --  Assumes Present (Rule), otherwise raises Fatal_Error.

   procedure Process_Rules (Ctx : in out Lkql_Context);
   --  Process input rules: Put the rules that have been requested by the user
   --  in internal data structures.

   procedure Clean_Up;
   --  Release all allocated ressources for rules and instances storage.

end Gnatcheck.Rules.Rule_Table;
