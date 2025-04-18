--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines "rules" for getting the information for the GNATCHECK
--  report from the check performed by the compiler.

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Gnatcheck.Rules; use Gnatcheck.Rules;

package Gnatcheck.Compiler is

   ---------------------
   -- Runtime helpers --
   ---------------------

   function Should_Use_Codepeer_Target return Boolean;
   --  TODO DOC.

   function GPRbuild_Exec return String;
   --  Return the executable name to use in order to spawn a GPRBuild process

   --------------------------------------------------------
   -- Using in GNATCHECK checks performed by the compiler --
   --------------------------------------------------------

   --  The compiler already performs a lot of checks that may be of interest
   --  for code certification and, therefore, it would be nice to perform
   --  these checks as a part of GNATCHECK run and to incorporate the results
   --  of these checks into the GNATCHECK report

   --  There are three kinds of the compiler-performed checks:
   --
   --  - checks initiated by the pragma Restriction_Warnings
   --  - style checks;
   --  - checks resulted in general (non-style) warnings;

   --  GNATCHECK uses three parametric rules to activate these checks:
   --  RESTRICTIONS, STYLE_CHECKS and WARNINGS, the parameters of these rules
   --  specify which restrictions, style rules and other conditions covered by
   --  compiler (non-style) warnings should be checked

   Use_gnaty_Option : Boolean := False;
   Use_gnatw_Option : Boolean := False;
   --  Boolean flags indicating if the corresponding GNAT option should be set

   Check_Restrictions : Boolean := False;
   --  Boolean flag indicating if the configuration file with
   --  Restriction_Warnings pragmas generated by gnatcheck should be used.

   procedure Set_Compiler_Checks;
   --  Sets the values of Use_gnaty_Option, Use_gnatw_Option and
   --  Check_Restrictions on the base of setting made by the warnings, style
   --  checks and restrictions gnatcheck options.

   Analyze_Compiler_Output : Boolean := False;
   --  Boolean flag indicating if gnatcheck should spawn the compiler and
   --  analyze its output.

   Gnatcheck_Config_File : String_Access :=
     new String'("restriction_pragmas.adc");
   --  The name of the file to place configuration pragmas gnatcheck needs to
   --  add the compiler checks to its report. This file always starts with:
   --
   --     pragma Warnings (Off, "[enabled by default]");
   --
   --  pragma needed to disable warnings that do not have a switch to turn the
   --  warning ON/OFF. If Restrictions rules are specified, this file contains
   --  the corresponding Restriction_Warnings pragmas.

   procedure Analyze_Output (File_Name : String; Errors : out Boolean);
   --  Parses the given file (typically error output of gprbuild or gnatcheck)
   --  and store all the relevant messages.
   --  If some compiler errors are detected, set Errors to True.

   procedure Process_Restriction_Param
     (Parameter : String; Instance : Rule_Instance_Access);
   --  Processes a parameter of a restriction (the restriction "rule") passed
   --  as an actual for Parameter. Only a single parameter should be passed,
   --  not a set of parameters separated by commas from the rule option.
   --  This procedure also populates the restriction tags map according to the
   --  given ``Instance``'s name.

   procedure Disable_Restrictions;
   --  Disable the "Restrictions" compiler-based rule by disabling all
   --  activated restrictions.

   procedure Process_Style_Check_Param
     (Param : String; Instance : Rule_Instance_Access);
   --  Processes a parameter of a style check (the style_check "rule") passed
   --  as an actual for Param. Only a single parameter should be passed, not a
   --  set of parameters separated by comma(s) from the rule option.
   --  This procedure also populates the style checks tags map according to
   --  the given ``Instance``'s name.

   procedure Disable_Style_Checks;
   --  Disable the "Style_Checks" rule and reset all registered style checking
   --  options.

   procedure Process_Warning_Param
     (Param : String; Instance : Rule_Instance_Access);
   --  Processes a parameter of a warning (the warning "rule") passed as an
   --  actual for Param. Only a single parameter should be passed, not a set of
   --  parameters separated by comma(s) from the rule option.
   --  This procedure also populates the warnings tags map according to the
   --  given ``Instance``'s name.

   procedure Disable_Warnings;
   --  Disable the "Warnings" rule and reset all registered warning options.

   procedure Create_Restriction_Pragmas_File;
   --  Creates in the temporary directory the configuration file containing
   --  the needed restriction pragmas

   function Get_Style_Option return String;
   --  Returns the '-gnatyxxx' option to be used in the compiler call, this
   --  function used the style check parameters saved as is, without any
   --  checks.

   function Get_Specified_Style_Option return String
   is (Get_Style_Option (7 .. Get_Style_Option'Last));
   --  Returns parameters of all the 'Style_Checks' rules specified for the
   --  given gnatcheck call, without adding anything to it and with no leading
   --  -gnaty.

   function Get_Warning_Option return String;
   --  Returns the '-gnatwxxx' option to be used in the compiler call.

   function Get_Specified_Warning_Option return String
   is (Get_Warning_Option (7 .. Get_Warning_Option'Last));
   --  Returns parameters of all the 'Warnings' rules specified for the given
   --  gnatcheck call, without adding anything to it and with no leading
   --  -gnatw.

   function Active_Restrictions_List
     (Separator : String; Elem_Prefix : String; Elem_Postfix : String)
      return String;
   --  Get the string representation of the active restriction list, formatted
   --  as required.

   procedure Print_Active_Restrictions (Ident_Level : Natural := 0);
   --  Prints out the Restriction Identifiers of the checks that are set active
   --  for the given gnatcheck call (with the corresponding parameter value, if
   --  any). Restriction identifiers are printed in a column, Ident_Level is
   --  used to control the indentation.

   procedure Print_Active_Restrictions_To_File (Rule_File : File_Type);
   --  Similar to the previous one, but prints the active restrictions in the
   --  format of restriction rules and places the output into the specified
   --  file that is supposed to be an opened out file.

   function Spawn_Gnatcheck_Worker
     (Rule_File   : String;
      Msg_File    : String;
      Source_File : String;
      Log_File    : String) return Process_Id;
   --  Spawn a gnatcheck worker (LKQL) on the main project file with the
   --  relevant options on the rules given by Rule_File, redirecting the
   --  output to Msg_File. Source_File is the name of a file listing all the
   --  source files to analyze. Log_File is the name of a file used to store
   --  worker's logs.

   function Spawn_LKQL_Rule_File_Parser
     (LKQL_RF_Name : String; Result_File : String) return Process_Id;
   --  Spawn the executable which handles the LKQL rule config file parsing
   --  with the provided `LKQL_RF_Name` then return the process identifier
   --  associated to it. Redirects all output made by the process in the
   --  `Result_File` file.

   function Spawn_GPRbuild (Output_File : String) return Process_Id;
   --  Spawn gprbuild on the main project file with the relevant options,
   --  redirecting the standard error to the given Output_File, to be used by
   --  Analyze_Output.

   procedure XML_Print_Active_Restrictions (Indent_Level : Natural := 0);
   --  Similar to the previous one, but prints the active restrictions from the
   --  coding standard in the the XMP report file.

   ------------------------------------------
   -- Routines for parametrized exemptions --
   ------------------------------------------

   function Is_Restriction_Active (Restriction_Name : String) return Boolean;
   --  Get whether the given restriction is active or not. If the
   --  ``Restriction_Name`` doesn't correspond to any restriction, this
   --  function returns False.

   function Is_Restriction_Exemption_Par (Par : String) return Boolean;
   --  Checks if Par can be used as a restriction rule parameter in the
   --  definition of exemption section. Assumes that Par is all lowercase and
   --  that Par does not contain any leading or trailing space.

   function Restriction_Rule_Parameter (Diag : String) return String;
   --  Assuming that Diag is a diagnosis string corresponding to a violation of
   --  some restriction-based rule, returns the parameter of the rule (used
   --  in parametrized exemption sections for restrictions)
   --
   --  Restriction violation diagnostics may have many formats, thus this
   --  function aims to support them all.
   --  Currently supported restriction violation formats are :
   --    * "Restriction_Name"
   --    * "Restriction_Name = Param"
   --    * "Restriction_Name => Param"

   function Is_Style_Exemption_Par (Par : String) return Boolean;
   --  Checks if Par can be used as a Style_Checks rule parameter in the
   --  definition of exemption section. Assumes that Par does not contain any
   --  leading or trailing space.

   function Is_Warning_Exemption_Par (Par : String) return Boolean;
   --  Checks if Par can be used as a Warnings rule parameter in the
   --  definition of exemption section. Assumes that Par does not contain any
   --  leading or trailing space.

   function Style_Rule_Parameter (Diag : String) return String;
   --  Assuming that Diag is a diagnosis string corresponding to a violation of
   --  some style-based rule, returns the parameter of the rule (used
   --  in parametrized exemption sections for style messages).

   function Warning_Rule_Parameter (Diag : String) return String;
   --  Assuming that Diag is a diagnosis string corresponding to a violation of
   --  some warning-based rule, returns the parameter of the rule (used
   --  in parametrized exemption sections for warnings).

end Gnatcheck.Compiler;
