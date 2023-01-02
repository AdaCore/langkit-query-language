------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                      G N A T C H E C K . R U L E S                       --
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

--  This is the top of gnatcheck hierarchy that defines individual rules, rule
--  table and rule checking process. It contains some basic type declarations

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with System.Rident;

with Liblkqllang.Analysis;
with Rule_Commands;           use Rule_Commands;

package Gnatcheck.Rules is

   pragma Warnings (Off);
   package Rident is new System.Rident;
   pragma Warnings (On);

   package Exemption_Parameters is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String);
   --  Needed to keep/process strings that can be used as rule parameters for
   --  rule exemptions.

   subtype Diagnosis_Variant is Natural;
   --  Used to numerate the variants of diagnostic message for the same rule.
   --  Zero means that the rule has exactly one variant of the diagnostic
   --  message

   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   -----------------
   -- Rule States --
   -----------------

   type Rule_States is (Enabled, Disabled);
   --  Each rule may have one of the following states:
   --   Enabled -  the rule should be checked at the moment, this state may
   --              be changed during the gnatcheck run (as a result of passing
   --              a control comment in the analyzed source)
   --   Disabled - the rule is disabled for the given gnatcheck run, this
   --              state cannot be changed during the gnatcheck run

   type Rule_Template is tagged record
      Name : String_Access;
      --  The only means of rule identification outside gnatcheck. All the
      --  rules implemented in gnatcheck should have unique names, the casing
      --  is not important.

      User_Synonym : String_Access;
      --  User-specified synonym for the rule name. It is used for
      --  documentation purposes only (to map gnatcheck rules onto rules from
      --  user's coding standard), it is not used for rule identification.

      Defined_At : String_Access;
      --  Location in the rule file where the rule has been enabled. Set to
      --  Nil_String_Loc if the rule has been enabled by command line option.

      Rule_State : Rule_States;
      --  Is the rule active or not

      Remediation_Level : Remediation_Levels;

      Help_Info : String_Access;
      --  Short help information for the rule

      Category : String_Access;
      --  Category for this rule

      Subcategory : String_Access;
      --  Subcategory for this rule, "" if none

      Follow_Instantiations : Boolean;
      --  True if this rule follows generic instantiations

      Allows_Parametrized_Exemption : Boolean;
      --  Whether you can specify a rule parameter when defining an exemption
      --  section for this Rule. In case if a rule parameter has the form like
      --  'Param_Name => Param_Value', in the exemption section you can specify
      --  only "Param_Name'

      Impact : Regexp_Access;
      --  For a KP detector, regexp to match relevant releases impacted, if
      --  any. Ignored if null.

      Target : Regexp_Access;
      --  For a KP detector, regexp to match relevant target triplets impacted,
      --  if any. Ignored if null.

      Parameters : Liblkqllang.Analysis.Parameter_Decl_List;
      --  List of formal parameters for this rule
   end record;

   type Rule_Access is access all Rule_Template'Class;

   --------------------------------------------------------
   -- Operations that may be redefined by specific rules --
   --------------------------------------------------------

   function Allowed_As_Exemption_Parameter
     (Ignored_Rule  : Rule_Template;
      Ignored_Param : String) return Boolean is (False);
   --  Checks if Param can be used as a rule parameter when defining an
   --  exemption section for Rule.

   function Rule_Parameter (Rule : Rule_Template; Diag : String) return String;
   --  Assuming that Allows_Parametrized_Exemption (Rule) and that Diag is a
   --  diagnosis generated for Rule (with all the variants and parameters
   --  resolved), defines the Rule parameter this Diag corresponds to. Returns
   --  an empty string if the definition is not possible because of any reason.

   procedure Init_Rule (Rule : in out Rule_Template);
   --  This is the means to provide the basic rule characteristics, such
   --  as rule name, texts of diagnostic and help messages, rule status etc.

   procedure Process_Rule_Parameter
     (Rule       : in out Rule_Template;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);
   --  Is supposed to process a single rule parameter of the gnatcheck call.
   --
   --  If the rule parameter string consists of more than one parameter
   --  separated by ',', then this procedure is called for each Param
   --  substring of the rule parameter that corresponds to the parts separated
   --  by commas. As the substring of the rule parameter, the Param string
   --  satisfies the following conditions:
   --
   --  * it starts from the first non-blank character after ',' or from the
   --    beginning of the rule parameter;
   --
   --  * it ends with the character immediately preceding the next ',';
   --
   --  * it contains at least one non-blank character;
   --
   --  * it does not contain a comma character;
   --
   --  The order of calling these routine for substrings of the rule parameter
   --  string corresponds to the order of this substrings in the rule parameter
   --  string. If the rule parameter string does not contain a comma character,
   --  this procedure is called only once for the whole rule parameter string
   --  without leading blank characters (if any).
   --
   --  See the body of Gnatcheck.Rules.Rule_Table.Process_Rule_Option for
   --  more details of parsing the rule parameter string.
   --
   --  Enable says if the given rule for the given parameter value
   --  should be set ON or OFF (the exact meaning of parameters and
   --  combinations of parameters values with Enable parameter should be
   --  defined individually for each rule). If the Param value (with the
   --  combination of Enable value) is not allowed for a given rule, this
   --  procedure should generate an error message (saying that the given
   --  parameter is ignored for a given rule) and does not do anything
   --  else.
   --
   --  Defined_At is used to store the place where the rule for the given
   --  parameter has been defined for the last time. It may be used to form a
   --  warning for redefinition of the rule parameter (this is not needed
   --  for all rules). The actual should have the standard GNAT format for
   --  SLOC file:line:columm if the rule with this parameter is defined in a
   --  rule file, or whould be empty if the rule option is given in a command
   --  line. (The following situation may be treated as a rule parameter
   --  redefinition: the rule is enabled with the value 'Val1' for parameter
   --  'Par1' and then it is enabled again with some (different or the same)
   --  value for this parameter. This looks suspicious and may result in
   --  warning. The decisions wether or not a warning should be issued in this
   --  sutuation are made on rule-by-rule basis. The case when the rule is
   --  enabled, then disabled and then enabled again does not result in
   --  a warning.)
   --
   --  The template does the following:
   --  - if Param string is empty, it sets the rule ON or OFF depending on the
   --    value of the Enable parameter;
   --
   --  - otherwise generates the error message saying that no parameter can be
   --    set for a given rule and that the parameter is ignored.

   procedure Map_Parameters
     (Rule : in out Rule_Template; Args : in out Rule_Argument_Vectors.Vector)
   is null;
   --  After parameters have been processed, store the relevant parameters for
   --  Rule in Map.

   procedure Print_Rule_Help (Rule : Rule_Template);
   --  Prints into Stderr the rule help information. If the help info is long,
   --  no wrapping is performed.

   function Has_Tip (Rule : Rule_Template) return Boolean;
   --  Tells if the rule XML help info should contain a <tip> tag. The template
   --  returns False. If this function is redefined for some rule, it should
   --  return True.

   procedure XML_Rule_Help (Rule : Rule_Template; Level : Natural);
   --  Prints the rule help information (including the information about rule
   --  parameters) in XML format, The template prints out the following string:
   --
   --  <check switch="+RRule_Name" label="brief rule help"/>

   procedure XML_Rule_Help_Tip (Rule : Rule_Template; Level : Natural);
   --  Prints the <tip> tag for the rule XML help info. The template does
   --  nothing.

   function Rule_Name (Rule : Rule_Template) return String;
   --  Returns the rule name

   function Is_Enabled (Rule : Rule_Template) return Boolean;
   --  Checks if the rule should be checked

   procedure Print_Rule_To_File
     (Rule         : Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints information about the (active) rule into the specified file. For
   --  the rules that have parameters this procedure should be rewritten to
   --  print out actual parameter settings.

   procedure XML_Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0);
   --  Similar to Print_Rule, but prints rule info into XML report file and
   --  does not add "+R" prefix to the rule name.

   function Annotate_Rule (Rule : Rule_Template) return String;
   --  If Gnatcheck.Options.Mapping_Mode is ON returns the rule annotation
   --  to be inserted into diagnosis, otherwise returns an empty string.

   function Has_Synonym (Rule : Rule_Template) return Boolean;
   --  Checks if the rule has a user-defined synonym.

   function Rule_Synonym (Rule : Rule_Template) return String;
   --  If Has_Synonym (Rule) then returns the rule synonym, otherwise returns
   --  an empty string.

   ----------------------------------
   -- "One integer parameter" rule --
   ----------------------------------

   type One_Integer_Parameter_Rule is new Rule_Template with record
      Param : Integer := Integer'First;
   end record;
   --  Param specifies the maximal (or minimal) allowed number of some
   --  property to be checked by the rule, this is supposed to be set by the
   --  rule parameter.

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);
   --  For '+R" option (Enable is ON) checks that the Param string represents
   --  an integer that is not less than Rule_Bound, and if it does, stores it
   --  as the value of the Rule_Limit field and turns the rule ON, otherwise
   --  generates the diagnostic message and turns the rule OFF.
   --  For '-R' option checks that Param string is empty and in case it is
   --  turns the rule OFF.
   --  Defined_At parameter is used to form warning in case of redefinition of
   --  the rule parameter.

   overriding procedure Map_Parameters
     (Rule : in out One_Integer_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Parameter_Rule;
      Indent_Level : Natural := 0);

   ----------------------------------
   -- "One boolean parameter" rule --
   ----------------------------------

   type Tri_State is (Unset, On, Off);

   type One_Boolean_Parameter_Rule is new Rule_Template with record
      Param : Tri_State := Unset;
   end record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Boolean_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : in out One_Boolean_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_Boolean_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : One_Boolean_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_Boolean_Parameter_Rule;
      Indent_Level : Natural := 0);

   ---------------------------------
   -- "One string parameter" rule --
   ---------------------------------

   type One_String_Parameter_Rule is new Rule_Template with record
      Param : Unbounded_Wide_Wide_String;
      File  : Unbounded_String;
   end record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_String_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : in out One_String_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_String_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : One_String_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_String_Parameter_Rule;
      Indent_Level : Natural := 0);

   --------------------------------
   -- "One array parameter" rule --
   --------------------------------

   type One_Array_Parameter_Rule is new One_String_Parameter_Rule
   with null record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Array_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : in out One_Array_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   -------------------------------------------------------
   -- "One Integer and/or Many Booleans parameter" rule --
   -------------------------------------------------------

   type Boolean_Parameters is array (2 .. 10) of Tri_State;

   type One_Integer_Or_Booleans_Parameter_Rule is new Rule_Template with record
      Integer_Param  : Integer := Integer'First;
      Boolean_Params : Boolean_Parameters := [others => Unset];
   end record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Or_Booleans_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : in out One_Integer_Or_Booleans_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Or_Booleans_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Or_Booleans_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Or_Booleans_Parameter_Rule;
      Indent_Level : Natural := 0);

   ------------------------------
   -- Identifier_Suffixes rule --
   ------------------------------

   type Identifier_Suffixes_Rule is new Rule_Template with record
      Type_Suffix,
      Access_Suffix,
      Access_Access_Suffix,
      Class_Access_Suffix,
      Class_Subtype_Suffix,
      Constant_Suffix,
      Renaming_Suffix,
      Access_Obj_Suffix,
      Interrupt_Suffix : Unbounded_Wide_Wide_String;
   end record;

   overriding function Rule_Parameter
     (Rule : Identifier_Suffixes_Rule;
      Diag : String) return String;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Suffixes_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Suffixes_Rule;
      Parameter : String) return Boolean is
   (To_Lower (Parameter) in
      "access" | "access_obj" | "class_access" | "class_subtype" |
      "constant" | "renaming" | "interrupt" | "type");

   overriding procedure Map_Parameters
     (Rule : in out Identifier_Suffixes_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Suffixes_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Suffixes_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Suffixes_Rule;
      Indent_Level : Natural := 0);

   ------------------------------
   -- Identifier_Prefixes rule --
   ------------------------------

   type Identifier_Prefixes_Rule is new Rule_Template with record
      Type_Prefix, Concurrent_Prefix,
      Access_Prefix, Class_Access_Prefix,
      Subprogram_Access_Prefix,
      Derived_Prefix, Constant_Prefix, Exception_Prefix,
      Enum_Prefix : Unbounded_Wide_Wide_String;
      Exclusive   : Tri_State := Unset;
   end record;

   overriding function Rule_Parameter
     (Rule : Identifier_Prefixes_Rule;
      Diag : String) return String;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Prefixes_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Prefixes_Rule;
      Parameter : String) return Boolean is
   (To_Lower (Parameter) in
      "type" | "concurrent" | "access" | "class_access" | "subprogram_access" |
      "derived" | "constant" | "enum" | "exception" | "exclusive");

   overriding procedure Map_Parameters
     (Rule : in out Identifier_Prefixes_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Prefixes_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Prefixes_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Prefixes_Rule;
      Indent_Level : Natural := 0);

   ----------------------------
   -- Identifier_Casing rule --
   ----------------------------

   type Identifier_Casing_Rule is new Rule_Template with record
      Type_Casing,
      Enum_Casing,
      Constant_Casing,
      Exception_Casing,
      Others_Casing,
      Exclude : Unbounded_Wide_Wide_String;
   end record;

   overriding function Rule_Parameter
     (Rule : Identifier_Casing_Rule;
      Diag : String) return String;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Casing_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding function Allowed_As_Exemption_Parameter
     (Rule      : Identifier_Casing_Rule;
      Parameter : String) return Boolean is
   (To_Lower (Parameter) in
      "type" | "constant" | "enum" | "exception" | "others" | "exclude");

   overriding procedure Map_Parameters
     (Rule : in out Identifier_Casing_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Casing_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Casing_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Casing_Rule;
      Indent_Level : Natural := 0);

   -----------------------
   -- Forbidden_* rules --
   -----------------------

   type Forbidden_Rule is new Rule_Template with record
      All_Flag  : Tri_State := Unset;
      Forbidden : Unbounded_Wide_Wide_String;
      Allowed   : Unbounded_Wide_Wide_String;
   end record;

   overriding function Rule_Parameter
     (Rule : Forbidden_Rule;
      Diag : String) return String;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Forbidden_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   function Allowed_As_Exemption_Parameter
     (Ignored_Rule  : Forbidden_Rule;
      Ignored_Param : String) return Boolean is (True);

   overriding procedure Map_Parameters
     (Rule : in out Forbidden_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : Forbidden_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : Forbidden_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Forbidden_Rule;
      Indent_Level : Natural := 0);

   ------------------------------------
   -- Silent_Exception_Handlers rule --
   ------------------------------------

   type Silent_Exception_Handlers_Rule is new Rule_Template with record
      Subprograms        : Unbounded_Wide_Wide_String;
      Subprogram_Regexps : Unbounded_Wide_Wide_String;
   end record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Silent_Exception_Handlers_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : in out Silent_Exception_Handlers_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : Silent_Exception_Handlers_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : Silent_Exception_Handlers_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Silent_Exception_Handlers_Rule;
      Indent_Level : Natural := 0);

   -----------------
   -- Custom rule --
   -----------------

   type Custom_Rule is new Rule_Template with record
      Arguments : Rule_Argument_Vectors.Vector;
   end record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Custom_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : in out Custom_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : Custom_Rule;
      Level : Natural);

   overriding procedure Print_Rule_To_File
     (Rule         : Custom_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : Custom_Rule;
      Indent_Level : Natural := 0);

end Gnatcheck.Rules;
