------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                      G N A T C H E C K . R U L E S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2004-2021, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

with Liblkqllang.Analysis;
with Rule_Commands;           use Rule_Commands;

package Gnatcheck.Rules is

   package Exemption_Parameters is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String);
   --  Needed to keep/process strings that can be used as rule parameters for
   --  rule exemptions.

   subtype Diagnosis_Variant is Natural;
   --  Used to numerate the variants of diagnostic message for the same rule.
   --  Zero means that the rule has exactly one variant of the diagnostic
   --  message

   subtype Rule_Name_Str  is String_Access;
   subtype Rule_Help      is String_Access;
   subtype Rule_Diagnosis is String_Access;
   --  Subtypes for storing the string information.

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

   type String_Loc is access all String;
   --  ###

   type Rule_Template is tagged record
      Name : Rule_Name_Str;
      --  The only means of rule identification outside gnatcheck. All the
      --  rules implemented in gnatcheck should have unique names, the casing
      --  is not important.

      Synonym : Rule_Name_Str;
      --  ###
      --  Synonym of the rule name. If we have to change the rule name, this
      --  synonym can be used for rule identification

      User_Synonym : Rule_Name_Str;
      --  User-specified synonym for the rule name. It is used for
      --  documentation purposes only (to map gnatcheck rules onto rules from
      --  user's coding standard), it is not used for rule identification.

      Defined_At : String_Loc;
      --  Location in the rule file where the rule has been enabled. Set to
      --  Nil_String_Loc if the rule has been enabled by command line option.

      Rule_State : Rule_States;
      --  Is the rule active or not

      Remediation_Level : Remediation_Levels;

      Help_Info : Rule_Help;
      --  Short help information for the rule

      Diagnosis : Rule_Diagnosis;
      --  Message generated in case if a rule violation is detected. A rule may
      --  have more than one diagnostic message associated with it. A
      --  diagnostic message may have formal parameters that should be replaced
      --  with some actual information computed from a specific rule violation.
      --  ### Do we need to keep this field

      Allows_Parametrized_Exemption : Boolean;
      --  Whether you can specify a rule parameter when defining an exemption
      --  section for this Rule. In case if a rule parameter has the form like
      --  'Param_Name => Param_Value', in the exemption section you can specify
      --  only "Param_Name'

      Parameters : Liblkqllang.Analysis.Parameter_Decl_List;
      --  List of formal parameters for this rule
   end record;

   type Rule_Access is access all Rule_Template'Class;

   --------------------------------------------------------
   -- Operations that may be redefined by specific rules --
   --------------------------------------------------------

   function Allowed_As_Exemption_Parameter
     (Ignored_Rule  : Rule_Template;
      Ignored_Param : String)
      return Boolean is (False);
   --  Checks if Param can be used as a rule parameter when defining an
   --  exemption section for Rule.

   function Rule_Parameter (Rule : Rule_Template; Diag : String) return String;
   --  Assuming that Allows_Prametrized_Exemption (Rule) and that Diag is a
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
     (Rule : Rule_Template; Args : in out Rule_Argument_Vectors.Vector)
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

   procedure Print_Rule (Rule : Rule_Template; Indent_Level : Natural := 0);
   --  Prints information about the (active) rule into the report file. For
   --  the rules that have parameters this procedure should be rewritten to
   --  print out actual parameter settings.
   --  This procedure does not close the last line it prints out.

   procedure Print_Rule_To_File
     (Rule         : Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Similar to Print_Rule, but prints rule information into the specified
   --  file. Rule_File is supposed to be an existing file opened as Out_File.
   --  This is not a good thing to have two procedures that do almost the same,
   --  the only reason why we do have them is historical.

   procedure XML_Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0);
   --  Similar to Print_Rule, but prints rule info into XML report file and
   --  does not add "+R" prefix to the rile name.

   function Rule_Option
     (Rule    : Rule_Template;
      Enabled : Boolean) return String;
   --  Returns the rule option (with the parameter, if needed) to be placed
   --  into the sample coding standard file (see Sample_Image). The result
   --  include "+R" or "-R" depending on Enabled. The template
   --  returns the rule name, it should be rewritten for the rules that have
   --  parameters.

   function Rule_Comment (Rule : Rule_Template) return String;
   --  Returns the comment note to be placed into the sample coding standard
   --  file (see Sample_Image), the result does not include "--". The template
   --  returns the content of the Help_Info field of the argument.

   function Annotate_Rule (Rule : Rule_Template) return String;
   --  If Gnatcheck.Options.Mapping_Mode is ON returns the rule annotation
   --  to be inserted into diagnosis, otherwise returns an empty string.

   function Has_Synonym (Rule : Rule_Template) return Boolean;
   --  Checks if the rule has a user-defined synonym.

   function Rule_Synonym (Rule : Rule_Template) return String;
   --  If Has_Synonym (Rule) then returns the rule synonym, otherwise returns
   --  an empty string.

   -----------------------------------
   -- "One integer parameter" rule" --
   -----------------------------------

   type One_Integer_Parameter_Rule is new Rule_Template with record
      Param : Integer;
   end record;
   --  Param specifies the maximal (or minimal) allowed number of some
   --  property to be checked by the rule, this is supposed to be set by the
   --  rule parameter.

   procedure Init_Rule (Rule : in out One_Integer_Parameter_Rule);

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
     (Rule : One_Integer_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule
     (Rule         : One_Integer_Parameter_Rule;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Parameter_Rule;
      Indent_Level : Natural := 0);

   overriding function Rule_Option
     (Rule    : One_Integer_Parameter_Rule;
      Enabled : Boolean) return String;

   -----------------------------------
   -- "One boolean parameter" rule" --
   -----------------------------------

   type Tri_State is (Unset, On, Off);

   type One_Boolean_Parameter_Rule is new Rule_Template with record
      Param : Tri_State;
   end record;
   --  Rule_Limit specifies the maximal (or minimal) allowed number of some
   --  property to be checked by the rule, this is supposed to be set by the
   --  rule parameter.

   overriding procedure Init_Rule
     (Rule : in out One_Boolean_Parameter_Rule);

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Boolean_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : One_Boolean_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_Boolean_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule
     (Rule         : One_Boolean_Parameter_Rule;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : One_Boolean_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_Boolean_Parameter_Rule;
      Indent_Level : Natural := 0);

   overriding function Rule_Option
     (Rule    : One_Boolean_Parameter_Rule;
      Enabled : Boolean) return String;

   ----------------------------------
   -- "One string parameter" rule" --
   ----------------------------------

   type One_String_Parameter_Rule is new Rule_Template with record
      Param : Unbounded_Wide_Wide_String;
   end record;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_String_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String);

   overriding procedure Map_Parameters
     (Rule : One_String_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector);

   overriding procedure XML_Rule_Help
     (Rule  : One_String_Parameter_Rule;
      Level : Natural);

   overriding procedure Print_Rule
     (Rule         : One_String_Parameter_Rule;
      Indent_Level : Natural := 0);

   overriding procedure Print_Rule_To_File
     (Rule         : One_String_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule
     (Rule         : One_String_Parameter_Rule;
      Indent_Level : Natural := 0);

   overriding function Rule_Option
     (Rule    : One_String_Parameter_Rule;
      Enabled : Boolean) return String;

   ----------------------------
   -- Common rule operations --
   ----------------------------

   procedure Sample_Image
     (Rule         : Rule_Template'Class;
      Enabled      : Boolean;
      Sample_File  : File_Type;
      Comment_From : Positive);
   --  Prints into Sample_File the rule option that turns this rule ON or OFF,
   --  depending on the value of Enabled. Used to create a template coding
   --  standard file.
   --  Comment_From indicates the position the comment that gives a short
   --  definition of the rule should start from

end Gnatcheck.Rules;
