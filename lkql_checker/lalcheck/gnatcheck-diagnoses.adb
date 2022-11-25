------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                  G N A T C H E C K . D I A G N O S E S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2005-2022, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;               use Ada.Calendar;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Wide_Wide_Characters.Handling;
use Ada.Wide_Wide_Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Case_Util;

with Gnatcheck.Output;             use Gnatcheck.Output;
with Gnatcheck.Projects.Aggregate; use Gnatcheck.Projects.Aggregate;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with Langkit_Support.Text; use Langkit_Support.Text;
with Libadalang.Common;
with Libadalang.Expr_Eval;

package body Gnatcheck.Diagnoses is

   package LCO renames Libadalang.Common;

   -----------------------
   -- Diagnoses storage --
   -----------------------

   type Diag_Message is record
      Text           : String_Access;
      Justification  : String_Access;
      Diagnosis_Kind : Diagnosis_Kinds;
      Rule           : Rule_Id;
      SF             : SF_Id;
   end record;

   function SLOC_Less_Than (L, R : String) return Boolean;
   --  Provided that L and R have the "file:line:col" format, compares these
   --  SLOCs alphabetically (first file names converted to lower case are
   --  compared, if they are equal, line numbers are compared, and if they are
   --  also equal colons are compared).
   --  If L or R does not have the proper format, the result is unpredictable.

   function Diag_Is_Less_Than (L, R : Diag_Message) return Boolean;
   --  If L or R is null, raises Constraint_Error. Otherwise compares SLOC
   --  parts of L and R (file names are converted to lower case).
   --
   --  If comparing SLOCs based on the rules given above results in the fact
   --  that these SLOCs are equal, the rest of the message is compared.

   function Diag_Is_Equal (L, R : Diag_Message) return Boolean;
   --  If L or R is null, raises Constraint_Error. Otherwise returns
   --  L.Text.all = R.Text.all.

   package Error_Messages_Storage is new Ada.Containers.Ordered_Sets
     (Element_Type => Diag_Message,
      "<"          => Diag_Is_Less_Than,
      "="          => Diag_Is_Equal);

   All_Error_Messages : Error_Messages_Storage.Set;

   Unused_Position : Error_Messages_Storage.Cursor;
   Unused_Inserted : Boolean;

   -------------------------------------------
   --  Local routines for diagnoses storage --
   -------------------------------------------

   procedure Compute_Statistics;
   --  Computes the number of violations and diagnoses of different kinds.
   --  Results are stored in the corresponding counters in the package spec.
   --  Also computes file statistics and stores it in the following counters.

   Checked_Sources                  : Natural := 0;
   Unverified_Sources               : Natural := 0;
   Fully_Compliant_Sources          : Natural := 0;
   Sources_With_Violations          : Natural := 0;
   Sources_With_Exempted_Violations : Natural := 0;
   Ignored_Sources                  : Natural := 0;

   -------------------------------------------
   --  Local routines for report generation --
   -------------------------------------------

   Rule_List_File_Name_Str           : constant String := "-rule-list";
   Source_List_File_Name_Str         : constant String := "-source-list";
   Ignored_Source_List_File_Name_Str : constant String :=
     "-ignored-source-list";

   function Auxiliary_List_File_Name (S : String) return String;
   --  Should be used for getting the names of auxiliary files needed for
   --  gnatchck report (list of processed sources, list of applied rules, list
   --  of ignored sources. Parameter specifies a substring to be included into
   --  the report file name to get the name of auxiliary file.

   Number : String_Access;
   pragma Unreferenced (Number);
   --  Used when processing individual projects as a part of aggregate project
   --  processing. Represents a numeric index (prepended by "_") that is
   --  taken from the report file name.

   procedure Copy_User_Info;
   --  Copies into the report file the text from user-provided file.

   function Escape_XML (S : String) return String;
   --  Escape relevant characters from S by their corresponding XML symbols

   procedure Print_Active_Rules_File;
   --  Prints the reference to the (actual argument or artificially created)
   --  file that contains the list of all the rules that are active for the
   --  given gnatcheck run.

   procedure Print_Argument_Files_Summary;
   --  Prints the total numbers of: all the argument files, non-compilable
   --  files, files with no violations, files with violations, files with
   --  exempted violations only.

   Diagnoses_To_Print : array (Rule_Violation .. Internal_Error)
     of Boolean := [others => False];
   --  Specifies which diagnoses should be printed out by the following
   --  procedure

   Print_Exempted_Violations : Boolean;
   --  Flag specifying if exempted or non-exempted violations should be
   --  printed. Has its effect only if Diagnoses_To_Print (Rule_Violation) is
   --  True.

   procedure Print_Diagnoses;
   --  Iterates through all the diagnoses and prints into the report file those
   --  of them, for which Diagnoses_To_Print is True (and the value of
   --  Print_Exempted_Violations either corresponds to the diagnosis or is not
   --  applicable for the diagnosis kind).

   procedure Print_File_List_File;
   --  Prints the reference to the (actual argument or artificially created)
   --  file that contains the list of all the files passed to gnatcheck

   procedure Print_Ignored_File_List_File;
   --  Prints the reference to the artificially created file that contains the
   --  list of files passed to gnatcheck that have not been processed because
   --  '--ignore=...' option. Note that it can be different from the list
   --  provided by '--ignore=...' option - this list contains only the existing
   --  files that have been passed as tool argument sources.

   procedure Print_Gnatcheck_Command_Line (XML : Boolean := False);
   --  Prints the gnatcheck command line. In case if gnatcheck has been
   --  called from the GNAT driver, prints the call to the GNAT driver, but not
   --  the gnatcheck call generated by the GNAT driver. If XML is ON, prints
   --  the output into XML output file, otherwise - in the text output file.

   procedure Print_Out_Diagnoses;
   --  Duplicates diagnoses about non-exempted rule violations, exemption
   --  warnings and compiler error messages into Stderr. Up to Max_Diagnoses
   --  diagnoses are reported. If Max_Diagnoses equal to 0, all the diagnoses
   --  of these kinds are reported.

   procedure Print_Runtime (XML : Boolean := False);
   --  Prints the runtime version used for gnatcheck call. It is either the
   --  parameter of --RTS option used for (actual) gnatcheck call or the
   --  "<default>" string if --RTS parameter is not specified. If XML is ON,
   --  prints the output into XML output file, otherwise - in the text output
   --  file.

   procedure Print_Violation_Summary;
   --  Prints the total numbers of: non-exempted)violations, exempted
   --  violations, exemption warnings and compiler errors.

   procedure XML_Report_Diagnosis
     (Diag         : Diag_Message;
      Short_Report : Boolean);
   --  Prints into XML report file the information from the diagnosis. The
   --  boolean parameter is used to define the needed indentation level

   function Select_Line   (Diag : String_Access) return Positive;
   function Select_Column (Diag : String_Access) return Positive;
   --  Assuming that Diag is a diagnosis, gets the line/column number from it.
   --  The result is undefined if Diag does not have a syntax of the diagnosis.

   function Strip_Warning (Diag : String) return String;
   --  Strip trailing " [-gnatwxxx]", if any

   ----------------------------------------------------------------------
   --  Data structures and local routines for rule exemption mechanism --
   ----------------------------------------------------------------------

   type Exemption_Kinds is (Not_An_Exemption, Exempt_On, Exempt_Off);

   function Get_Exemption_Kind
     (Image : Wide_Wide_String) return Exemption_Kinds;
   --  Returns Exemption_Kinds value represented by Image. Returns
   --  Not_An_Exemption if Image does not represent a valid exemption kind.

   type Exemption_Info is record
      Line_Start : Natural;
      Col_Start  : Natural;
      --  Location of exemption pragma that turns exemption ON

      Line_End : Natural;
      Col_End  : Natural;
      --  End of the exemption section

      Justification : String_Access;

      Detected : Natural;
      --  Number of the diagnoses generated for exempted rule
   end record;

   type Exemption_Sections_Array is array (Rule_Id range <>) of Exemption_Info;
   type Exemption_Sections_Array_Access is access Exemption_Sections_Array;
   Exemption_Sections : Exemption_Sections_Array_Access;
   --  Storage for currently processed exemption sections. Should have a
   --  separate entry for each rule. (We cannot allocate it statically because
   --  of elaboration problems - we do not know how many rules we have unlit
   --  all of them are registered).

   function Is_Exempted (Rule : Rule_Id) return Boolean;
   --  Checks if Rule is in exempted state. Assumes Present (Rule).

   procedure Process_Postponed_Exemptions;
   --  Iterate through the stored diagnoses and apply postponed exemptions to
   --  diagnoses.

   procedure Turn_Off_Exemption
     (Rule         : Rule_Id;
      Closing_Span : Source_Location_Range;
      SF           : SF_Id);
   --  Cleans up the stored exemption section for the argument Rule.

   -----------------------------
   -- Parametric exemptions --
   -----------------------------

   Rule_Exemption_Parameters : Exemption_Parameters.Set;
   --  Temporary place for collecting rule parameters in case if exemption
   --  specifies parameters for a rule.

   type Parametrized_Exemption_Info is record
      Exempt_Info : Exemption_Info;
      Rule        : Rule_Id;
      SF          : SF_Id;
      Params      : String_List_Access;
   end record;

   type Param_Ex_Info_Key is record
      SF         : SF_Id;
      Line_Start : Natural;
      Col_Start  : Natural;
   end record;

   function Key
     (Element : Parametrized_Exemption_Info) return Param_Ex_Info_Key
   is (SF         => Element.SF,
       Line_Start => Element.Exempt_Info.Line_Start,
       Col_Start  => Element.Exempt_Info.Col_Start);

   function "<" (L, R : Param_Ex_Info_Key) return Boolean is
     (L.SF < R.SF
      or else (L.SF = R.SF
               and then (L.Line_Start < R.Line_Start
                         or else (L.Line_Start = R.Line_Start
                                  and then L.Col_Start < R.Col_Start))));

   function Is_Equal (L, R : String_List_Access) return Boolean;
   --  Checks if L and R have the same content.

   function Is_Equal
     (L    : Exemption_Parameters.Set;
      R    : String_List_Access) return Boolean;
   --  Checks that L and R are both non-empty/non-null and have the same
   --  content.

   function Belongs
     (L : String;
      R : String_List_Access) return Boolean;
   --  Checks if R is non-null and L belongs to R

   function To_Pars_List
     (Pars : Exemption_Parameters.Set) return String_List_Access;
   --  Converts (non-empty! the caller is responsible for that!) set of rule
   --  exemption parameters into string list.

   function Params_Img
     (Params : String_List_Access;
      Rule   : Rule_Id) return String;
   --  Returns the string image of rule exemption parameters that are supposed
   --  to be stored in Params list (and Params is supposed to be not null) in
   --  the format suitable for including in a diagnosis. Rule parameter is
   --  needed to decide if we should fold the parameter in proper case (for
   --  sure we should not do this for Warnings rule)

   function "=" (L, R : Parametrized_Exemption_Info) return Boolean is
     (L.SF = R.SF                                         and then
      Is_Equal (L.Params, R.Params)                       and then
      L.Exempt_Info.Line_Start = R.Exempt_Info.Line_Start and then
      L.Exempt_Info.Col_Start  = R.Exempt_Info.Col_Start);

   function "<" (L, R : Parametrized_Exemption_Info) return Boolean is
     (L.SF < R.SF
      or else (L.SF = R.SF
               and then
                 (L.Exempt_Info.Line_Start < R.Exempt_Info.Line_Start
                  or else (L.Exempt_Info.Line_Start = R.Exempt_Info.Line_Start
                           and then L.Exempt_Info.Col_Start
                                      < R.Exempt_Info.Col_Start))));

   package Parametrized_Exemption_Sections is new
     Ada.Containers.Ordered_Sets (Parametrized_Exemption_Info);

   package Exem_Section_Keys is new
     Parametrized_Exemption_Sections.Generic_Keys
       (Key_Type => Param_Ex_Info_Key,
        Key      => Key);

   type Rule_Param_Exempt_Sections_Type is array (Rule_Id range <>) of
     Parametrized_Exemption_Sections.Set;

   type Rule_Param_Exempt_Sections_Access is access
     Rule_Param_Exempt_Sections_Type;

   Rule_Param_Exempt_Sections : Rule_Param_Exempt_Sections_Access;
   --  Stores parametric exemption sections for a source being processed

   procedure Set_Rule_Exempt_Pars
     (Rule : Rule_Id;
      Pars : String;
      SF   : SF_Id;
      SLOC : String);
   --  Assuming that Rule is a rule that allows parametric expressions,
   --  and Pars is a part of a third argument of a rule exemption pragma that
   --  contains rule parameters, parses Pars string and checks if each of the
   --  specified parameters indeed can be used as rule exemption parameter.
   --  If all the parameters can be used in rule exemption, all of them are
   --  placed into the value of Rule_Exemption_Parameters set variable. If
   --  some parameter is wrong, and error message is generated and
   --  Rule_Exemption_Parameters sets to empty set. Note that if immediately
   --  after a call to this subprogram Rule_Exemption_Parameters is empty, then
   --  the corresponding exemption pragma is incorrect.
   --  Parameters SF and SLOC are needed to form an error message.

   function Allows_Parametrized_Exemption (Rule : Rule_Id) return Boolean;
   --  Checks if Rule allows fine-tuned exemption (with specifying parameters
   --  for that it can be exempted). Assumes Present (Rule).

   function Allowed_As_Exemption_Parameter
     (Rule  : Rule_Id;
      Param : String) return Boolean;
   --  Checks if Param is allowed as a rule parameter in rule exemption pragma.
   --  Assumes that Param has already folded to lower case. Always returns
   --  False if Allows_Prametrized_Exemption (Rule) is False.

   procedure Is_Exempted_With_Pars
     (Rule        : Rule_Id;
      Exempted_At : out Parametrized_Exemption_Sections.Cursor);
   --  Checks if Rule is already exempted with the set of parameters that are
   --  stored in the global Rule_Exemption_Parameters variable. If it does then
   --  Exempted_At is set to the set cursor pointing to the corresponding
   --  exemption section in Rule_Param_Exempt_Sections, otherwise Exempted_At
   --  is set to No_Element. The check is made for the parametric exemption
   --  sections that are currently opened in the source being traversed and
   --  stored in Rule_Param_Exempt_Sections (see below). This procedure is
   --  supposed to use only in Process_Exemption_Pragma, it depends on the
   --  specific calling context created when analyzing a rule exemption pragma.

   procedure Same_Parameter_Exempted
     (Rule        : Rule_Id;
      Exempted_At : out Parametrized_Exemption_Sections.Cursor;
      Par         : out String_Access);
   --  Is similar to the previous procedure, but it checks that there is at
   --  least one parameter in Rule_Exemption_Parameters that has already been
   --  used in definition of some opened parametric exception section for
   --  this rule:
   --
   --     Exempt_On, Rule:Par1, Par3
   --     ...
   --     Exempt_On, Rule:Par1, Par2
   --
   --  If the checks succeeds, Par is set to point to this parameter. The
   --  caller is responsible for freeing the memory occupied by Par.

   function Get_Param_Justification
     (Rule : Rule_Id;
      Diag : String;
      SF   : SF_Id;
      Line : Natural;
      Col  : Natural) return String_Access;
   --  Diag is supposed to be a diagnostic message (with all the parameters
   --  and variants resolved) generated for Rule (and Rule should allow
   --  parametric exemptions). It checks if Text corresponds to some
   --  parametric section for this Rule.
   --
   --  If Diag fits into an exemption section, the result is the pointer to the
   --  corresponding justification and the diagnostic counter for this section
   --  is increased as a side effect of the function call. Otherwise the result
   --  is null.
   --
   --  Parameters Line and Col should be set to the line and column numbers
   --  from the diagnosis to filter out sections the diagnosis does not get
   --  into.

   function Rule_Parameter (Diag : String; Rule : Rule_Id) return String;
   --  Provided that Rule allows parametric exemptions, and  Diag is a
   --  diagnostic message corresponding to this rule, defines the rule
   --  exemption parameter Diag corresponds to.

   function Get_Exem_Section
     (Exem_Sections : Parametrized_Exemption_Sections.Set;
      Param         : String;
      Line          : Natural;
      Col           : Natural) return Parametrized_Exemption_Sections.Cursor;
   --  Tries to locate in Exem_Sections the section that exempts the rule this
   --  Param is supposed to corresponds to (choosing the right set of exemption
   --  sections should be done outside the call) with Param. Returns
   --  No_Element if the attempt fails.

   procedure Increase_Diag_Counter
     (Exem_Sections : in out Parametrized_Exemption_Sections.Set;
      Section       : Parametrized_Exemption_Sections.Cursor);
   --  Adds 1 to the counter of detected violations for the exemption sections
   --  pointed by Section in Exem_Sections.

   procedure Turn_Off_Parametrized_Exemption
     (Rule         : Rule_Id;
      Exempted_At  : in out Parametrized_Exemption_Sections.Cursor;
      Closing_Span : Source_Location_Range;
      SF           : SF_Id);
   --  Cleans up the stored exemption section for the argument Rule.

   -------------------------------------
   -- Exemptions for postponed checks --
   -------------------------------------

   type Postponed_Rule_Exemption_Info;
   type Postponed_Rule_Exemption_Info_Access is access
     Postponed_Rule_Exemption_Info;

   type Postponed_Rule_Exemption_Info is record
      Exemption_Section      : Exemption_Info;
      Next_Exemption_Section : Postponed_Rule_Exemption_Info_Access;
   end record;

   type Postponed_Check_Exemption_Sections_Array is array (SF_Id range <>) of
     Postponed_Rule_Exemption_Info_Access;

   type Postponed_Check_Exemption_Sections_Array_Access is access
     Postponed_Check_Exemption_Sections_Array;

   type Postponed_Exemption_Sections_Array is array (Rule_Id range <>) of
     Postponed_Check_Exemption_Sections_Array_Access;

   type Postponed_Exemption_Sections_Array_Access is access
     Postponed_Exemption_Sections_Array;

   Postponed_Exemption_Sections : Postponed_Exemption_Sections_Array_Access;
   --  For each argument source, stores all the exemption sections found in
   --  this source. These sections are stored as they are processed - that is,
   --  in alphabetical order. Sections for different kinds of checks are stored
   --  separately.

   procedure Map_On_Postponed_Check_Exemption
     (In_File       : SF_Id;
      For_Check     : Rule_Id;
      For_Line      : Positive;
      Is_Exempted   : out Boolean;
      Justification : in out String_Access);
   --  This procedure checks if For_Line parameter gets into the corresponding
   --  exemption section and sets Is_Exempted accordingly.
   --  If Is_Exempted is set to True, Justification is set to the relevant
   --  Justification.

   ------------------------------------------------
   -- Parametric exemptions for postponed checks --
   ------------------------------------------------

   type Per_Source_Postponed_Param_Exemp is array (SF_Id range <>)
     of Parametrized_Exemption_Sections.Set;

   type Per_Source_Postponed_Param_Exemp_Access is access
     Per_Source_Postponed_Param_Exemp;

   type Per_Rule_Postponed_Param_Exemp is array (Rule_Id range <>) of
     Per_Source_Postponed_Param_Exemp_Access;

   type Per_Rule_Postponed_Param_Exemp_Access is access
     Per_Rule_Postponed_Param_Exemp;

   Postponed_Param_Exempt_Sections : Per_Rule_Postponed_Param_Exemp_Access;
   --  Here we collect all the parametric exemption sections for postponed
   --  checks.

   --------------------------------------------
   -- Debug routines for exemption mechanism --
   --------------------------------------------

   procedure Parametrized_Exem_Section_Debug_Image
     (S : Parametrized_Exemption_Sections.Cursor);
   --  Prints out the debug image of the information stored for a parametric
   --  exemption section.

   procedure Rule_Parametrized_Exem_Sections_Debug_Image (Rule : Rule_Id);
   pragma Unreferenced (Rule_Parametrized_Exem_Sections_Debug_Image);
   --  Prints out the debug image of the current state of the information about
   --  parametric exemption sections stored for Rule in
   --  Rule_Param_Exempt_Sections

   ------------------------------------
   -- Allowed_As_Exemption_Parameter --
   ------------------------------------

   function Allowed_As_Exemption_Parameter
     (Rule  : Rule_Id;
      Param : String)
      return  Boolean
   is
      Result : Boolean := False;
   begin
      if not Allows_Parametrized_Exemption (Rule) then
         return False;
      end if;

      case Rule is
         when Restrictions_Id =>
            Result := Is_Restriction_Exemption_Par (Param);
         when Style_Checks_Id =>
            null;
         when Warnings_Id =>
            Result := Is_Warning_Exemption_Par (Param);
         when others =>
            Result :=
              Allowed_As_Exemption_Parameter
                (All_Rules.Table (Rule).all, Param);
      end case;

      return Result;
   end Allowed_As_Exemption_Parameter;

   -----------------------------------
   -- Allows_Parametrized_Exemption --
   -----------------------------------

   function Allows_Parametrized_Exemption (Rule : Rule_Id) return Boolean is
      Result : Boolean := False;
   begin
      case Rule is
         when Restrictions_Id | Warnings_Id => Result := True;
         when Style_Checks_Id               => null;
         when others                        =>
            Result := All_Rules.Table (Rule).Allows_Parametrized_Exemption;
      end case;

      return Result;
   end Allows_Parametrized_Exemption;

   function Belongs
     (L : String;
      R : String_List_Access) return Boolean
   is
      Result : Boolean := False;
   begin
      if R /= null then
         for J in R'Range loop
            if L = R (J).all then
               Result := True;
               exit;
            elsif R (J).all > L then
               exit;
            end if;
         end loop;
      end if;

      return Result;
   end Belongs;

   ------------------------------
   -- Auxiliary_List_File_Name --
   ------------------------------

   function Auxiliary_List_File_Name (S : String) return String is
      Prj_Out_File   : constant String  :=
       (if Text_Report_ON then Base_Name (Get_Report_File_Name)
        else                   Base_Name (Get_XML_Report_File_Name));
      Prj_Out_First  : constant Natural := Prj_Out_File'First;
      Prj_Out_Last   : constant Natural := Prj_Out_File'Last;
      Prj_Out_Dot    :          Natural := Index (Prj_Out_File, ".", Backward);
      Prj_Out_Suffix : constant String :=
        (if Prj_Out_Dot = 0 then ""
         else Prj_Out_File (Prj_Out_Dot .. Prj_Out_Last));

      Suff_Start : Natural;
      Suff_End   : Natural;

   begin
      if Prj_Out_Dot = 0 then
         Prj_Out_Dot := Prj_Out_Last;
      else
         Prj_Out_Dot := Prj_Out_Dot - 1;
      end if;

      if Aggregated_Project then
         --  in case of aggregated project we have to move the index in the
         --  Prj_Out_File after S. That is, we do not need
         --  gnatcheck_1-source-list.out, we need gnatcheck-source-list_1.out
         --  for the sake of upward compatibility

         Suff_Start :=
           Index (Prj_Out_File (Prj_Out_First .. Prj_Out_Dot), "_",  Backward);
         Suff_End := Prj_Out_Dot;

         return Prj_Out_File (Prj_Out_First .. Suff_Start - 1) & S &
                Prj_Out_File (Suff_Start .. Suff_End) & Prj_Out_Suffix;
      else

         return Prj_Out_File (Prj_Out_First .. Prj_Out_Dot) & S &
                Prj_Out_Suffix;
      end if;
   end Auxiliary_List_File_Name;

   ------------------------------------
   -- Check_Unclosed_Rule_Exemptions --
   ------------------------------------

   procedure Check_Unclosed_Rule_Exemptions
     (SF   : SF_Id;
      Unit : LAL.Analysis.Analysis_Unit)
   is
      Comp_Span : constant Source_Location_Range := Unit.Root.Sloc_Range;
      use Parametrized_Exemption_Sections;

      Next_Section : Cursor;
   begin
      --  Non-parametric exemptions:

      for Rule in Exemption_Sections'Range loop
         if Is_Exempted (Rule) then
            Store_Diagnosis
              (Text               =>
                 File_Name (SF) & ':' &
                 Sloc_Image (Exemption_Sections (Rule).Line_Start,
                             Exemption_Sections (Rule).Col_Start) &
                 ": no matching 'exempt_OFF' annotation for rule " &
                 Rule_Name (Rule),
               Diagnosis_Kind     => Exemption_Warning,
               SF                 => SF);

            Turn_Off_Exemption
              (Rule         => Rule,
               Closing_Span => Comp_Span,
               SF           => SF);
         end if;
      end loop;

      --  Parametric exemptions:

      for Rule in Rule_Param_Exempt_Sections'Range loop
         if Allows_Parametrized_Exemption (Rule)
           and then not Is_Empty (Rule_Param_Exempt_Sections (Rule))
         then
          --  We cannot use set iterator here - we need to use Rule and SF
          --  into processing routine

            Next_Section := First (Rule_Param_Exempt_Sections (Rule));

            while Has_Element (Next_Section) loop
               Store_Diagnosis
                 (Text               =>
                    Short_Source_Name (SF) & ':'                    &
                    Sloc_Image (Parametrized_Exemption_Sections.Element
                                  (Next_Section).Exempt_Info.Line_Start,
                                Parametrized_Exemption_Sections.Element
                                  (Next_Section).Exempt_Info.Col_Start) &
                    ": no matching 'exempt_OFF' annotation for rule " &
                    Rule_Name (Rule),
                  Diagnosis_Kind     => Exemption_Warning,
                  SF                 => SF);
               Turn_Off_Parametrized_Exemption
                 (Rule, Next_Section, Comp_Span, SF);
               Next_Section := First (Rule_Param_Exempt_Sections (Rule));
            end loop;
         end if;
      end loop;
   end Check_Unclosed_Rule_Exemptions;

   ------------------------
   -- Compute_Statistics --
   ------------------------

   procedure Compute_Statistics is
      type Violations_Detected is record
         Exempted_Violations_Detected     : Boolean := False;
         Non_Exempted_Violations_Detected : Boolean := False;
      end record;

      File_Counter : array (First_SF_Id .. Last_Argument_Source) of
        Violations_Detected := [others => (False, False)];

      procedure Count_Diagnoses (Position : Error_Messages_Storage.Cursor);

      procedure Count_Diagnoses (Position : Error_Messages_Storage.Cursor) is
         SF : constant SF_Id := Error_Messages_Storage.Element (Position).SF;
      begin
         if not Is_Argument_Source (SF) then
            --  All the statistics is collected for argument files only
            return;
         end if;

         case Error_Messages_Storage.Element (Position).Diagnosis_Kind is
            when Not_A_Diagnosis =>
               pragma Assert (False);
               null;
            when Rule_Violation =>
               if Error_Messages_Storage.Element (Position).Justification =
                  null
               then
                  Detected_Non_Exempted_Violations := @ + 1;
                  File_Counter (SF).Non_Exempted_Violations_Detected := True;
               else
                  Detected_Exempted_Violations := @ + 1;
                  File_Counter (SF).Exempted_Violations_Detected := True;
               end if;
            when Exemption_Warning =>
               Detected_Exemption_Warning := @ + 1;
            when Compiler_Error =>
               Detected_Compiler_Error := @ + 1;
            when Internal_Error =>
               Detected_Internal_Error := @ + 1;
         end case;
      end Count_Diagnoses;

   begin
      Error_Messages_Storage.Iterate
        (Container => All_Error_Messages,
         Process   => Count_Diagnoses'Access);

      for SF in First_SF_Id .. Last_Argument_Source loop
         if Source_Status (SF) in Not_A_Legal_Source | Error_Detected then
            Unverified_Sources := @ + 1;
         else
            Checked_Sources := @ + 1;

            if File_Counter (SF).Non_Exempted_Violations_Detected then
               Sources_With_Violations := @ + 1;
            elsif File_Counter (SF).Exempted_Violations_Detected then
               Sources_With_Exempted_Violations := @ + 1;
            elsif Source_Status (SF) = Processed then
               Fully_Compliant_Sources := @ + 1;
            end if;
         end if;
      end loop;
   end Compute_Statistics;

   --------------------
   -- Copy_User_Info --
   --------------------

   procedure Copy_User_Info is
      Max_Line_Len : constant Positive := 1024;
      Line_Buf     :          String (1 .. Max_Line_Len);
      Line_Len     :          Natural;
      User_File    :          Ada.Text_IO.File_Type;
   begin
      --  Very simple-minded implementation...

      Open (File => User_File,
            Mode => In_File,
            Name => User_Info_File_Full_Path.all);

      loop
         exit when End_Of_File (User_File);

         Get_Line (File => User_File,
                   Item => Line_Buf,
                   Last => Line_Len);

         if Text_Report_ON then
            Report (Line_Buf (1 .. Line_Len));
         end if;

         if XML_Report_ON then
            XML_Report (Line_Buf (1 .. Line_Len), 1);
         end if;
      end loop;

      Close (User_File);
   exception
      when E : others =>

         Report_EOL;
         Report ("cannot successfully copy information " &
                 "from " & User_Info_File.all);

         if Is_Open (User_File) then
            Close (User_File);
         end if;

         Error ("cannot copy information from " & User_Info_File.all &
                " into report file");

         Error_No_Tool_Name (Ada.Exceptions.Exception_Information (E));
   end Copy_User_Info;

   -------------------
   -- Diag_Is_Equal --
   -------------------

   function Diag_Is_Equal (L, R : Diag_Message) return Boolean is
   begin
      return L.Text.all = R.Text.all;
   end Diag_Is_Equal;

   ----------------
   -- Escape_XML --
   ----------------

   function Escape_XML (S : String) return String is
      Result : Unbounded_String;
      use Ada.Strings.Unbounded;
   begin
      for C of S loop
         case C is
            when '&' =>
               Append (Result, "&amp;");
            when '<' =>
               Append (Result, "&lt;");
            when '>' =>
               Append (Result, "&gt;");
            when '"' =>
               Append (Result, "&quot;");
            when ASCII.NUL .. ASCII.US =>
               declare
                  Img : constant String := Integer'Image (Character'Pos (C));
               begin
                  Append
                    (Result, "&#" & Img (Img'First + 1 .. Img'Last) & ";");
               end;
            when others =>
               Append (Result, C);
         end case;
      end loop;

      return To_String (Result);
   end Escape_XML;

   --------------------
   -- SLOC_Less_Than --
   --------------------

   function SLOC_Less_Than (L, R : String) return Boolean is
      L_Start      : Natural := L'First;
      R_Start      : Natural := R'First;

      L_End        : Natural := Index (L, ":");
      R_End        : Natural := Index (R, ":");
      L_Val        : Positive;
      R_Val        : Positive;
      L_Has_Column : Boolean := True;
      R_Has_Column : Boolean := True;

   begin
      --  When comparing SLOCs we have the following problems:
      --  1. file names may be in different casing, and X.ads < a.ads for
      --     predefined "<"
      --
      --  2. ':' that separates file name, line and column is greater than
      --     any digit, so for the predefined "<" a.ads:50:20 is less than
      --     a.ads 5:20
      --
      --  3. we cannot use alphabetical comparison for the whole SLOC, because
      --     line and column numbers should be compared as digits
      --
      --  4. if Full_Source_Locations is ON, in Windows we may have ':' as a
      --     part of a full file name.

      if L_End = L_Start + 1
        and then L (L_End + 1) in '\' | '/'
      then
         L_End := Index (L (L_End + 2 .. L'Last), ":");
      end if;

      if R_End = R_Start + 1
        and then R (R_End + 1) in '\' | '/'
      then
         R_End := Index (R (R_End + 2 .. R'Last), ":");
      end if;

      if L_End = 0 or else R_End = 0 then
         return False;
      elsif To_Lower (L (L_Start .. L_End)) < To_Lower (R (R_Start .. R_End))
      then
         return True;
      elsif To_Lower (L (L_Start .. L_End)) > To_Lower (R (R_Start .. R_End))
      then
         return False;
      else
         --  file names are the same, we have to compare line and column
         --  numbers
         L_Start := L_End + 1;
         R_Start := R_End + 1;

         L_End := Index (L (L_Start .. L'Last), ":");

         if L_End = 0 then
            L_End        := L'Last;
            L_Has_Column := False;
         else
            L_End   := L_End - 1;
         end if;

         R_End := Index (R (R_Start .. R'Last), ":");

         if R_End = 0 then
            R_End        := R'Last;
            R_Has_Column := False;
         else
            R_End   := R_End - 1;
         end if;

         L_Val := Positive'Value (L (L_Start .. L_End));
         R_Val := Positive'Value (R (R_Start .. R_End));

         if L_Val < R_Val then
            return True;
         elsif L_Val > R_Val then
            return False;
         else
            --  line numbers are also the same, comparing columns if both
            --  SLOCs have them (which should always be the case).

            if not (L_Has_Column or else R_Has_Column) then
               return False;
            elsif not L_Has_Column and then R_Has_Column then
               return True;
            elsif L_Has_Column and then not L_Has_Column then
               return False;
            end if;

            L_Val := Positive'Value (L (L_End + 2 .. L'Last));
            R_Val := Positive'Value (R (R_End + 2 .. R'Last));

            return L_Val < R_Val;
         end if;
      end if;
   end SLOC_Less_Than;

   -----------------------
   -- Diag_Is_Less_Than --
   -----------------------

   function Diag_Is_Less_Than (L, R : Diag_Message) return Boolean is
      L_First : constant Natural := L.Text'First;
      R_First : constant Natural := R.Text'First;

      L_SLOC_End : constant Natural := Index (L.Text.all, ": ") - 1;
      R_SLOC_End : constant Natural := Index (R.Text.all, ": ") - 1;

   begin
      if Diag_Is_Equal (L, R) then
         return False;
      end if;

      if L.Text (L_First .. L_SLOC_End) /= R.Text (R_First .. R_SLOC_End) then
         return SLOC_Less_Than (L.Text (L_First .. L_SLOC_End),
                                R.Text (R_First .. R_SLOC_End));
      else
         --  If we are here, we have equal SLOCs, so compare messages

         return L.Text (L_SLOC_End + 1 .. L.Text'Last)
                  < R.Text (R_SLOC_End + 1 .. R.Text'Last);
      end if;
   end Diag_Is_Less_Than;

   -----------------------------
   -- Exemption_Justification --
   -----------------------------

   function Exemption_Justification (Rule : Rule_Id) return String_Access is
   begin
      return Exemption_Sections (Rule).Justification;
   end Exemption_Justification;

   -----------------------------------
   -- Generate_Qualification_Report --
   -----------------------------------

   procedure Generate_Qualification_Report is
   begin
      Number := new String'(Get_Number);
      Ignored_Sources := Exempted_Sources;

      Process_Postponed_Exemptions;
      Compute_Statistics;

      if XML_Report_ON then
         XML_Report ("<?xml version=""1.0""?>");
         XML_Report_No_EOL ("<gnatcheck-report");

         if Gnatcheck_Prj.Is_Specified then
            XML_Report (" project=""" &
              (if Aggregated_Project then
                  Get_Aggregated_Project
               else
                  Gnatcheck_Prj.Source_Prj) & """>");
         else
            XML_Report (">");
         end if;
      end if;

      --  OVERVIEW

      if not Short_Report then
         Print_Report_Header;
         Print_Active_Rules_File;
         Print_File_List_File;
         Print_Ignored_File_List_File;
         Print_Argument_Files_Summary;

         if Text_Report_ON then
            Report_EOL;
         end if;

         Print_Violation_Summary;

         --  2. DETECTED EXEMPTED RULE VIOLATIONS
         if Text_Report_ON then
            Report_EOL;
            Report ("2. Exempted Coding Standard Violations");
            Report_EOL;
         end if;

         if XML_Report_ON then
            XML_Report ("<violations>");
         end if;
      end if;

      if Detected_Exempted_Violations > 0 then
         Diagnoses_To_Print := [Rule_Violation    => True,
                                Exemption_Warning => False,
                                Compiler_Error    => False,
                                Internal_Error    => False];
         Print_Exempted_Violations := True;
         Print_Diagnoses;

      else
         if Text_Report_ON then
            Report ("no exempted violations detected", 1);
         end if;

         if not Short_Report and then XML_Report_ON then
            XML_Report ("no exempted violations detected", 1);
         end if;
      end if;

      if not Short_Report then
         if Text_Report_ON then
            Report_EOL;
            Report ("3. Non-exempted Coding Standard Violations");
            Report_EOL;
         end if;
      end if;

      if Detected_Non_Exempted_Violations > 0 then
         Diagnoses_To_Print := [Rule_Violation    => True,
                                Exemption_Warning => False,
                                Compiler_Error    => False,
                                Internal_Error    => False];
         Print_Exempted_Violations := False;
         Print_Diagnoses;

      else
         if Text_Report_ON then
            Report ("no non-exempted violations detected", 1);
         end if;

         if not Short_Report and then XML_Report_ON then
            XML_Report ("no non-exempted violations detected", 1);
         end if;
      end if;

      if not Short_Report then
         if Text_Report_ON then
            Report_EOL;
            Report ("4. Rule exemption problems");
            Report_EOL;
         end if;
      end if;

      if Detected_Exemption_Warning > 0 then
         Diagnoses_To_Print := [Rule_Violation    => False,
                                Exemption_Warning => True,
                                Compiler_Error    => False,
                                Internal_Error    => False];
         Print_Diagnoses;

      else
         if Text_Report_ON then
            Report ("no rule exemption problems detected", 1);
         end if;

         if not Short_Report and then XML_Report_ON then
            XML_Report ("no rule exemption problems detected", 1);
         end if;

      end if;

      if not Short_Report then
         if Text_Report_ON then
            Report_EOL;
            Report ("5. Language violations");
            Report_EOL;
         end if;
      end if;

      if Detected_Compiler_Error > 0 then
         Diagnoses_To_Print := [Rule_Violation    => False,
                                Exemption_Warning => False,
                                Compiler_Error    => True,
                                Internal_Error    => False];
         Print_Diagnoses;

      else
         if Text_Report_ON then
            Report ("no language violations detected", 1);
         end if;

         if not Short_Report and then XML_Report_ON then
            XML_Report ("no language violations detected", 1);
         end if;
      end if;

      if not Short_Report then
         if Text_Report_ON then
            Report_EOL;
            Report ("6. Gnatcheck internal errors");
            Report_EOL;
         end if;
      end if;

      if Detected_Internal_Error > 0 then
         Diagnoses_To_Print := [Rule_Violation    => False,
                                Exemption_Warning => False,
                                Compiler_Error    => False,
                                Internal_Error    => True];
         Print_Diagnoses;

      else
         if Text_Report_ON then
            Report ("no internal error detected", 1);
         end if;

         if not Short_Report and then XML_Report_ON then
            XML_Report ("no internal error detected", 1);
         end if;
      end if;

      --  User-defined part

      if not Short_Report then
         if XML_Report_ON then
            XML_Report ("</violations>");
         end if;

         if Text_Report_ON then
            Report_EOL;
         end if;

         if User_Info_File /= null then
            if Text_Report_ON then
               Report ("7. Additional Information");
               Report_EOL;
            end if;

            if XML_Report_ON then
               XML_Report ("<additional-information>");
            end if;

            Copy_User_Info;

            if Text_Report_ON then
               Report_EOL;
            end if;

            if XML_Report_ON then
               XML_Report ("</additional-information>");
            end if;
         end if;
      end if;

      if XML_Report_ON then
         XML_Report ("</gnatcheck-report>");
      end if;

      --  Sending the diagnoses into Stderr

      if Brief_Mode or not Quiet_Mode then
         Print_Out_Diagnoses;
      end if;
   end Generate_Qualification_Report;

   ----------------------
   -- Get_Exem_Section --
   ----------------------

   function Get_Exem_Section
     (Exem_Sections : Parametrized_Exemption_Sections.Set;
      Param         : String;
      Line          : Natural;
      Col           : Natural) return Parametrized_Exemption_Sections.Cursor
   is
      use Parametrized_Exemption_Sections;
      Result               : Cursor  := No_Element;
      Next_Section         : Cursor  := First (Exem_Sections);

      Diag_In_Section      : Boolean := True;
      Diag_Before_Sections : Boolean := False;
      --  Control iterating through all the parametric exemption
      --  sections for the source that are stored in Exem_Sections

      Next_Section_El : Parametrized_Exemption_Info;
   begin
      while Has_Element (Next_Section) loop
         Next_Section_El :=
           Parametrized_Exemption_Sections.Element (Next_Section);

         Diag_Before_Sections :=
           Line < Next_Section_El.Exempt_Info.Line_Start
             or else (Line = Next_Section_El.Exempt_Info.Line_Start
                      and then Col < Next_Section_El.Exempt_Info.Col_Start);

         exit when Diag_Before_Sections;

         Diag_In_Section :=
           (Line > Next_Section_El.Exempt_Info.Line_Start
            or else (Line = Next_Section_El.Exempt_Info.Line_Start
                     and then Col > Next_Section_El.Exempt_Info.Col_Start))
             and then (Line < Next_Section_El.Exempt_Info.Line_End
                       or else (Line = Next_Section_El.Exempt_Info.Line_End
                                and then
                                  Col < Next_Section_El.Exempt_Info.Col_End));

         if Diag_In_Section
           and then Belongs (Param, Next_Section_El.Params)
         then
            Result := Next_Section;
            exit;
         end if;

         Next_Section := Next (Next_Section);
      end loop;

      return Result;
   end Get_Exem_Section;

   ------------------------
   -- Get_Exemption_Kind --
   ------------------------

   function Get_Exemption_Kind
     (Image : Wide_Wide_String) return Exemption_Kinds
   is
      Result : Exemption_Kinds;
   begin
      if Image (Image'First) = '"' then

         --  Old format of Annotate pragma. We have to cut out quotation marks

         Result :=
           Exemption_Kinds'Wide_Wide_Value
             (Image (Image'First + 1 .. Image'Last - 1));
      else
         Result := Exemption_Kinds'Wide_Wide_Value (Image);
      end if;

      return Result;

   exception
      when Constraint_Error =>
         return Not_An_Exemption;
   end Get_Exemption_Kind;

   -----------------------------
   -- Get_Param_Justification --
   -----------------------------

   function Get_Param_Justification
     (Rule : Rule_Id;
      Diag : String;
      SF   : SF_Id;
      Line : Natural;
      Col  : Natural) return String_Access
   is
      Result : String_Access;

      use Parametrized_Exemption_Sections;
      Exem_Sections  : access Parametrized_Exemption_Sections.Set;
      --  You should never assign containers!!!
      Fit_In_Section : Cursor;
   begin
      declare
         Param : constant String := Rule_Parameter (Diag, Rule);
         pragma Assert (Param /= "" or else Rule = Warnings_Id);
      begin
         Exem_Sections :=
           Postponed_Param_Exempt_Sections (Rule) (SF)'Unrestricted_Access;
         Fit_In_Section := Get_Exem_Section
           (Exem_Sections.all, Param, Line, Col);

         if Has_Element (Fit_In_Section) then
            Result :=
              Parametrized_Exemption_Sections.Element (Fit_In_Section).
                Exempt_Info.Justification;
            Increase_Diag_Counter (Exem_Sections.all, Fit_In_Section);
         end if;
      end;

      return Result;
   end Get_Param_Justification;

   ---------------------------
   -- Increase_Diag_Counter --
   ---------------------------

   procedure Increase_Diag_Counter
     (Exem_Sections : in out Parametrized_Exemption_Sections.Set;
      Section       :        Parametrized_Exemption_Sections.Cursor)
   is
      procedure Add_One (Exem_Section : in out Parametrized_Exemption_Info);

      procedure Add_One (Exem_Section : in out Parametrized_Exemption_Info) is
      begin
         Exem_Section.Exempt_Info.Detected := @ + 1;
      end Add_One;
   begin
      Exem_Section_Keys.Update_Element_Preserving_Key
        (Container => Exem_Sections,
         Position  => Section,
         Process   => Add_One'Access);
   end Increase_Diag_Counter;

   ---------------------
   -- Init_Exemptions --
   ---------------------

   procedure Init_Exemptions is
   begin
      Exemption_Sections :=
        new Exemption_Sections_Array (First_Compiler_Check .. All_Rules.Last);

      Exemption_Sections.all := [others =>
        (Line_Start    => 0,
         Col_Start     => 0,
         Line_End      => 0,
         Col_End       => 0,
         Justification => null,
         Detected      => 0)];

      Postponed_Exemption_Sections :=
        new Postponed_Exemption_Sections_Array
          (First_Compiler_Check .. All_Rules.Last);

      for Rule in Postponed_Exemption_Sections'Range loop
         Postponed_Exemption_Sections (Rule) :=
           new Postponed_Check_Exemption_Sections_Array
             (First_SF_Id .. Last_Argument_Source);
      end loop;

      --  Parametric exemptions:

      Rule_Param_Exempt_Sections :=
        new Rule_Param_Exempt_Sections_Type
          (First_Compiler_Check .. All_Rules.Last);

      Postponed_Param_Exempt_Sections :=
        new Per_Rule_Postponed_Param_Exemp
          (First_Compiler_Check .. All_Rules.Last);

      for Rule in Postponed_Exemption_Sections'Range loop
         if Allows_Parametrized_Exemption (Rule) then
            Postponed_Param_Exempt_Sections (Rule) :=
              new Per_Source_Postponed_Param_Exemp
                (First_SF_Id .. Last_Argument_Source);
         end if;
      end loop;
   end Init_Exemptions;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (L, R : String_List_Access) return Boolean is
      Result : Boolean := (L = null) and (R = null);
      R_Idx  : Natural;
   begin
      if L /= null and then
         R /= null and then
         L'Length = R'Length
      then
         R_Idx  := R'First;
         Result := True;

         for J in L'Range loop
            if L (J) = null or else R (R_Idx) = null then
               --  Even if both elements are null we return False because this
               --  cannot be considered as the same sequence of parameters
               Result := False;
               exit;
            elsif L (J).all /= R (R_Idx).all then
               Result := False;
               exit;
            end if;

            R_Idx := @ + 1;
         end loop;
      end if;

      return Result;
   end Is_Equal;

   function Is_Equal
     (L    : Exemption_Parameters.Set;
      R    : String_List_Access)
      return Boolean
   is
      use Exemption_Parameters;
      Result : Boolean := not Is_Empty (L) and then
                          R /= null        and then
                          Natural (Length (L)) = R'Length;
   begin
      if Result then
         declare
            L_Arr : String_List_Access := To_Pars_List (L);
         begin
            Result := Is_Equal (L_Arr, R);
            Free (L_Arr);
         end;
      end if;

      return Result;
   end Is_Equal;

   -----------------
   -- Is_Exempted --
   -----------------

   function Is_Exempted (Rule : Rule_Id) return Boolean is
   begin
      return Exemption_Sections (Rule).Line_Start > 0;
   end Is_Exempted;

   ---------------------------
   -- Is_Exempted_With_Pars --
   ---------------------------

   procedure Is_Exempted_With_Pars
     (Rule        : Rule_Id;
      Exempted_At : out Parametrized_Exemption_Sections.Cursor)
   is
      use Parametrized_Exemption_Sections;
      Next_Section : Cursor;
   begin
      Exempted_At := No_Element;

      if not Is_Empty (Rule_Param_Exempt_Sections (Rule)) then
         Next_Section := First (Rule_Param_Exempt_Sections (Rule));

         while Has_Element (Next_Section) loop
            if Is_Equal
                 (Rule_Exemption_Parameters,
                  Parametrized_Exemption_Sections.Element
                    (Next_Section).Params)
            then
               Exempted_At := Next_Section;
               exit;
            end if;

            Next_Section := Next (Next_Section);
         end loop;
      end if;
   end Is_Exempted_With_Pars;

   ---------------------------------------
   -- Map_On_Postponed_Check_Exemption --
   ---------------------------------------

   procedure Map_On_Postponed_Check_Exemption
     (In_File       : SF_Id;
      For_Check     : Rule_Id;
      For_Line      : Positive;
      Is_Exempted   : out Boolean;
      Justification : in out String_Access)
   is
      Section : Postponed_Rule_Exemption_Info_Access;
   begin
      Is_Exempted := False;

      if not Is_Argument_Source (In_File) then
         --  Exemption sections are processed in argument files only!
         return;
      end if;

      Section := Postponed_Exemption_Sections (For_Check) (In_File);

      --  Traverse exemption section chain

      while Section /= null loop
         if For_Line in Section.Exemption_Section.Line_Start ..
                        Section.Exemption_Section.Line_End
         then
            Is_Exempted := True;
            Section.Exemption_Section.Detected := @ + 1;
            Justification := Section.Exemption_Section.Justification;
            exit;
         end if;

         exit when For_Line > Section.Exemption_Section.Line_End;

         Section := @.Next_Exemption_Section;
      end loop;
   end Map_On_Postponed_Check_Exemption;

   -------------------------------------------
   -- Parametrized_Exem_Section_Debug_Image --
   -------------------------------------------

   procedure Parametrized_Exem_Section_Debug_Image
     (S : Parametrized_Exemption_Sections.Cursor)
   is
      ES_Info : Parametrized_Exemption_Info;
   begin
      if Parametrized_Exemption_Sections.Has_Element (S) then
         ES_Info := Parametrized_Exemption_Sections.Element (S);

         Info ("+++++++++++++++++++++++++++++++++++++++++++++++");
         Info_No_EOL ("parametric exemption section for rule");
         Info (ES_Info.Rule'Img & "(" & Rule_Name (ES_Info.Rule) & ")");
         Info ("in file SF=" & ES_Info.SF'Img &
               "(" & Short_Source_Name (ES_Info.SF) & ")");
         Info ("Parameters:");

         if ES_Info.Params = null then
            Info ("   Null parameter list!!!");
         elsif ES_Info.Params'Length = 0 then
            Info ("   Empty parameter list!!!");
         else
            for J in ES_Info.Params'Range loop
               Info (">>" & ES_Info.Params (J).all & "<<");
            end loop;
         end if;

         Info ("Span: " & ES_Info.Exempt_Info.Line_Start'Img & ":" &
                ES_Info.Exempt_Info.Col_Start'Img & " -"  &
                ES_Info.Exempt_Info.Line_End'Img & ":" &
                ES_Info.Exempt_Info.Col_End'Img);
         Info ("Detected :" & ES_Info.Exempt_Info.Detected'Img);
         Info_No_EOL ("Justification: ");

         if ES_Info.Exempt_Info.Justification = null then
            Info ("IS NOT SET!!!");
         else
            Info (">>" & ES_Info.Exempt_Info.Justification.all & "<<");
         end if;
      else
         Info ("No parametric exemption section");
      end if;
   end Parametrized_Exem_Section_Debug_Image;

   ----------------
   -- Params_Img --
   ----------------

   function Params_Img
     (Params : String_List_Access;
      Rule   : Rule_Id)
      return   String
   is
      Res_Len : Natural := 0;
   begin
      pragma Assert (Params /= null);

      for J in Params'Range loop
         Res_Len := Res_Len + Params (J)'Length;
      end loop;

      Res_Len := Res_Len + (Params'Length - 1) * 2;  --  for ", "

      declare
         Result      : String (1 .. Res_Len);
         Append_From : Natural := Result'First;
      begin
         for J in Params'Range loop
            if J > Params'First then
               Result (Append_From .. Append_From + 1) := ", ";
               Append_From := Append_From + 2;
            end if;

            Result (Append_From .. Append_From + Params (J)'Length - 1) :=
              (if Rule = Warnings_Id
               then Params (J).all
               else GNAT.Case_Util.To_Mixed (Params (J).all));
            Append_From := Append_From + Params (J)'Length;
         end loop;

         return Result;
      end;
   end Params_Img;

   -----------------------------
   -- Print_Active_Rules_File --
   -----------------------------

   procedure Print_Active_Rules_File is
      Rule_List_File : Ada.Text_IO.File_Type;
   begin
      if Text_Report_ON then
         Report_No_EOL ("coding standard   : ");
      end if;

      if XML_Report_ON then
         XML_Report_No_EOL
           ("<coding-standard from-file=""", Indent_Level => 1);
      end if;

      if not Individual_Rules_Set
        and then
         Rule_File_Name /= null
      then
         if Text_Report_ON then
            Report (Rule_File_Name.all);
         end if;

         if XML_Report_ON then
            XML_Report (Rule_File_Name.all & """>");
         end if;
      else
         --  Creating the list of active rules

         declare
            Full_Rule_List_File_Name : constant String :=
              (if Get_Report_File_Name /= "" then
                  GNAT.Directory_Operations.Dir_Name (Get_Report_File_Name)
               else
                  GNAT.Directory_Operations.Dir_Name
                    (Get_XML_Report_File_Name)) &
              Auxiliary_List_File_Name (Rule_List_File_Name_Str);

         begin
            if Is_Regular_File (Full_Rule_List_File_Name) then
               Open (Rule_List_File, Out_File, Full_Rule_List_File_Name);
            else
               Create (Rule_List_File, Out_File, Full_Rule_List_File_Name);
            end if;

            for Rule in All_Rules.First .. All_Rules.Last loop
               if Is_Enabled (All_Rules.Table (Rule).all) then
                  Print_Rule_To_File
                    (All_Rules.Table (Rule).all, Rule_List_File);
                  New_Line (Rule_List_File);
               end if;
            end loop;

            --  Compiler-made checks:

            if Use_gnaty_Option then
               New_Line (Rule_List_File);
               Put_Line (Rule_List_File, "-- Compiler style checks:");
               Put      (Rule_List_File, "+RStyle_Checks : ");
               Put_Line (Rule_List_File,
                         Get_Style_Option
                           (Get_Style_Option'First + 6 ..
                              Get_Style_Option'Last));
            end if;

            if Use_gnatw_Option then
               New_Line (Rule_List_File);
               Put_Line (Rule_List_File, "--  Compiler warnings:");
               Put      (Rule_List_File, "+RWarnings : ");
               Put_Line (Rule_List_File, Get_Specified_Warning_Option);
            end if;

            if Check_Restrictions then
               New_Line (Rule_List_File);
               Put_Line (Rule_List_File, "--  Compiler restrictions:");
               Print_Active_Restrictions_To_File (Rule_List_File);
            end if;

            Close (Rule_List_File);

            if Text_Report_ON then
               Report (Auxiliary_List_File_Name (Rule_List_File_Name_Str));
            end if;

            if XML_Report_ON then
               XML_Report
                 (Auxiliary_List_File_Name (Rule_List_File_Name_Str) & """>");
            end if;
         end;
      end if;

      if XML_Report_ON then
         for Rule in All_Rules.First .. All_Rules.Last loop
            if Is_Enabled (All_Rules.Table (Rule).all) then
               XML_Print_Rule (All_Rules.Table (Rule).all, Indent_Level => 2);
            end if;
         end loop;

         if Use_gnaty_Option then
            XML_Report
              ("<rule id=""Style_Checks"">",
               Indent_Level => 2);
            XML_Report
              ("<parameter>" &
                 Get_Style_Option
                   (Get_Style_Option'First + 6 ..
                      Get_Style_Option'Last) & "</parameter>",
               Indent_Level => 3);
            XML_Report ("</rule>", Indent_Level => 2);
         end if;

         if Use_gnatw_Option then
            XML_Report
              ("<rule id=""Warnings"">",
               Indent_Level => 2);
            XML_Report
              ("<parameter>" & Get_Specified_Warning_Option & "</parameter>",
               Indent_Level => 3);
            XML_Report ("</rule>", Indent_Level => 2);
         end if;

         if Check_Restrictions then
            XML_Print_Active_Restrictions (Indent_Level => 1);
         end if;

         XML_Report ("</coding-standard>", Indent_Level => 1);
      end if;
   end Print_Active_Rules_File;

   ----------------------------------
   -- Print_Argument_Files_Summary --
   ----------------------------------

   procedure Print_Argument_Files_Summary is
   begin

      if Text_Report_ON then
         Report ("1. Summary");
         Report_EOL;

         Report ("fully compliant sources               :" &
                 Fully_Compliant_Sources'Img, 1);
         Report ("sources with exempted violations only :" &
                 Sources_With_Exempted_Violations'Img, 1);
         Report ("sources with non-exempted violations  :" &
                 Sources_With_Violations'Img, 1);
         Report ("unverified sources                    :" &
                 Unverified_Sources'Img, 1);
         Report ("total sources                         :" &
                 Last_Argument_Source'Img, 1);
         Report ("ignored sources                       :" &
                 Ignored_Sources'Img, 1);
      end if;

      if XML_Report_ON then
         XML_Report ("<summary>");

         XML_Report ("<fully-compliant-sources>"     &
                     Image (Fully_Compliant_Sources) &
                     "</fully-compliant-sources>",
                     Indent_Level => 1);

         XML_Report ("<sources-with-exempted-violations-only>"     &
                     Image (Sources_With_Exempted_Violations) &
                     "</sources-with-exempted-violations-only>",
                     Indent_Level => 1);

         XML_Report ("<sources-with-non-exempted-violations>"     &
                     Image (Sources_With_Violations) &
                     "</sources-with-non-exempted-violations>",
                     Indent_Level => 1);

         XML_Report ("<unverified-sources>"     &
                     Image (Unverified_Sources) &
                     "</unverified-sources>",
                     Indent_Level => 1);

         XML_Report ("<total-sources>"     &
                     Image (Integer (Last_Argument_Source)) &
                     "</total-sources>",
                     Indent_Level => 1);

      end if;

      pragma Assert (Checked_Sources =
                       Fully_Compliant_Sources +
                       Sources_With_Violations +
                       Sources_With_Exempted_Violations +
                       Ignored_Sources);
      pragma Assert (Natural (Last_Argument_Source) =
                       Checked_Sources + Unverified_Sources);
   end Print_Argument_Files_Summary;

   ---------------------
   -- Print_Diagnoses --
   ---------------------

   procedure Print_Diagnoses is

      procedure Print_Specified_Diagnoses
        (Position : Error_Messages_Storage.Cursor);
      --  Print the given message if relevant.
      --  Iterator for Error_Messages_Storage

      -------------------------------
      -- Print_Specified_Diagnoses --
      -------------------------------

      procedure Print_Specified_Diagnoses
        (Position : Error_Messages_Storage.Cursor)
      is
         Diag : constant Diag_Message :=
           Error_Messages_Storage.Element (Position);
      begin
         if Diagnoses_To_Print (Diag.Diagnosis_Kind) then
            if Diag.Diagnosis_Kind = Rule_Violation
              and then Print_Exempted_Violations = (Diag.Justification = null)
            then
               return;
            end if;

            if Text_Report_ON then
               Report (Strip_Warning (Diag.Text.all));

               if Diag.Justification /= null then
                  Report ("(" & Diag.Justification.all & ")", 1);
               end if;
            end if;

            if XML_Report_ON then
               XML_Report_Diagnosis (Diag, Short_Report);
            end if;
         end if;
      end Print_Specified_Diagnoses;

   begin
      Error_Messages_Storage.Iterate
        (Container => All_Error_Messages,
         Process   => Print_Specified_Diagnoses'Access);
   end Print_Diagnoses;

   --------------------------
   -- Print_File_List_File --
   --------------------------

   procedure Print_File_List_File is
      Source_List_File : Ada.Text_IO.File_Type;
   begin
      if Text_Report_ON then
         Report_No_EOL ("list of sources   : ");
      end if;

      if XML_Report_ON then
         XML_Report_No_EOL ("<sources from-file=""", Indent_Level => 1);
      end if;

      --  Creating the list of processed sources

      declare
         Full_Source_List_File_Name : constant String :=
           (if Get_Report_File_Name /= "" then
               GNAT.Directory_Operations.Dir_Name (Get_Report_File_Name)
            else
               GNAT.Directory_Operations.Dir_Name
                 (Get_XML_Report_File_Name)) &
           Auxiliary_List_File_Name (Source_List_File_Name_Str);

      begin
         if XML_Report_ON then
            XML_Report
              (Auxiliary_List_File_Name (Source_List_File_Name_Str) & """>");
         end if;

         if Is_Regular_File (Full_Source_List_File_Name) then
            Open
              (Source_List_File,
               Out_File,
               Full_Source_List_File_Name);
         else
            Create
              (Source_List_File,
               Out_File,
               Full_Source_List_File_Name);
         end if;

         for SF in First_SF_Id .. Last_Argument_Source loop
            if Source_Info (SF) /= Ignore_Unit then
               Put_Line (Source_List_File, Short_Source_Name (SF));
            end if;
         end loop;

         Close (Source_List_File);

         if Text_Report_ON then
            Report (Auxiliary_List_File_Name (Source_List_File_Name_Str));
         end if;
      end;

      if Text_Report_ON then
         Report_EOL;
      end if;

      if XML_Report_ON then
         for SF in First_SF_Id .. Last_Argument_Source loop
            if XML_Report_ON and then Source_Info (SF) /= Ignore_Unit then
               XML_Report
                 ("<source>" & Source_Name (SF) & "</source>",
                  Indent_Level => 2);
            end if;
         end loop;

         XML_Report ("</sources>", Indent_Level => 1);
      end if;
   end Print_File_List_File;

   ----------------------------------
   -- Print_Gnatcheck_Command_Line --
   ----------------------------------

   procedure Print_Gnatcheck_Command_Line (XML : Boolean := False) is
   begin
      if XML then
         XML_Report_No_EOL (Ada.Command_Line.Command_Name);

         for Arg in 1 .. Ada.Command_Line.Argument_Count loop
            XML_Report_No_EOL (" " & Ada.Command_Line.Argument (Arg));
         end loop;
      else
         Report_No_EOL (Ada.Command_Line.Command_Name);

         for Arg in 1 .. Ada.Command_Line.Argument_Count loop
            Report_No_EOL (" " & Ada.Command_Line.Argument (Arg));
         end loop;

         Report_EOL;
      end if;
   end Print_Gnatcheck_Command_Line;

   ----------------------------------
   -- Print_Ignored_File_List_File --
   ----------------------------------

   procedure Print_Ignored_File_List_File is
      Ignored_Source_List_File : Ada.Text_IO.File_Type;
   begin
      if Ignored_Sources = 0 then
         return;
      end if;

      if Text_Report_ON then
         Report_No_EOL ("list of ignored sources   : ");
      end if;

      if XML_Report_ON then
         XML_Report_No_EOL ("<ignored-sources from-file=""",
                            Indent_Level => 1);
      end if;

      declare
         Full_Ignored_Source_List_File_Name : constant String :=
           (if Get_Report_File_Name /= "" then
                  GNAT.Directory_Operations.Dir_Name (Get_Report_File_Name)
               else
                  GNAT.Directory_Operations.Dir_Name
                    (Get_XML_Report_File_Name)) &
           Auxiliary_List_File_Name (Ignored_Source_List_File_Name_Str);

      begin
         if XML_Report_ON then
            XML_Report
              (Auxiliary_List_File_Name (Ignored_Source_List_File_Name_Str) &
               """>");
         end if;

         if Is_Regular_File (Full_Ignored_Source_List_File_Name) then
            Open
              (Ignored_Source_List_File,
               Out_File,
               Full_Ignored_Source_List_File_Name);
         else
            Create
              (Ignored_Source_List_File,
               Out_File,
               Full_Ignored_Source_List_File_Name);
         end if;

         for SF in First_SF_Id .. Last_Argument_Source loop
            if Source_Info (SF) = Ignore_Unit then
               Put_Line (Ignored_Source_List_File, Short_Source_Name (SF));
            end if;
         end loop;

         Close (Ignored_Source_List_File);

         if Text_Report_ON then
            Report
             (Auxiliary_List_File_Name (Ignored_Source_List_File_Name_Str));
         end if;
      end;

      if Text_Report_ON then
         Report_EOL;
      end if;

      if XML_Report_ON then
         for SF in First_SF_Id .. Last_Argument_Source loop
            if XML_Report_ON and then Source_Info (SF) = Ignore_Unit then
               XML_Report
                 ("<source>" & Source_Name (SF) & "</source>",
                  Indent_Level => 2);
            end if;
         end loop;

         XML_Report ("</ignored-sources>", Indent_Level => 1);
      end if;
   end Print_Ignored_File_List_File;

   -------------------------
   -- Print_Out_Diagnoses --
   -------------------------

   procedure Print_Out_Diagnoses is
      Diagnoses_Reported : Natural := 0;
      Limit_Exceeded     : Boolean := False;
      GPS_Prefix         : constant String  := "check:";

      function Preprocess_Diag (Diag : String) return String;
      --  Add GPS_Prefix if Progress_Indicator_Mode is True, and remove any
      --  training [-gnatwxxx].

      procedure Counted_Print_Diagnosis
        (Position : Error_Messages_Storage.Cursor);
      --  Print Diagnosis until reaching Max_Diagnoses

      ---------------------
      -- Preprocess_Diag --
      ---------------------

      function Preprocess_Diag (Diag : String) return String is
      begin
         if Progress_Indicator_Mode then
            declare
               Idx : constant Natural := Index (Diag, ": ");
            begin
               return Diag (Diag'First .. Idx + 1) & GPS_Prefix & ' ' &
                            Strip_Warning (Diag (Idx + 2 .. Diag'Last));
            end;
         else
            return Strip_Warning (Diag);
         end if;
      end Preprocess_Diag;

      -----------------------------
      -- Counted_Print_Diagnosis --
      -----------------------------

      procedure Counted_Print_Diagnosis
        (Position : Error_Messages_Storage.Cursor) is
      begin
         if not Limit_Exceeded then
            if Max_Diagnoses > 0 and then
               Diagnoses_Reported > Max_Diagnoses
            then
               Limit_Exceeded := True;
               Info ("Maximum diagnoses reached, " &
                     "see the report file for full details");
            else
               if Error_Messages_Storage.Element (Position).Justification
                    = null
               then
                  Diagnoses_Reported := @ + 1;
                  Info
                    (Preprocess_Diag
                      (Error_Messages_Storage.Element (Position).Text.all));
               end if;
            end if;
         end if;
      end Counted_Print_Diagnosis;

   begin
      Error_Messages_Storage.Iterate
        (Container => All_Error_Messages,
         Process   => Counted_Print_Diagnosis'Access);
   end Print_Out_Diagnoses;

   -------------------------
   -- Print_Report_Header --
   -------------------------

   procedure Print_Report_Header is
      Time_Of_Check   : constant Time := Clock;
      Month_Of_Check  : constant Month_Number := Month (Time_Of_Check);
      Day_Of_Check    : constant Day_Number   := Day (Time_Of_Check);
      Sec_Of_Check    : constant Day_Duration := Seconds (Time_Of_Check);

      Sec_Of_Day      :          Integer := Integer (Sec_Of_Check);
      Hour_Of_Check   :          Integer range 0 .. 23;
      Minute_Of_Check :          Integer range 0 .. 59;
      Seconds_In_Hour : constant Integer := 60 * 60;

   begin
      if Sec_Of_Day = 86400 then
         --  This happens when Sec_Of_Check is very close to the end of the
         --  day (it is in 86399.5 .. 86400.0). We treat this situation as the
         --  last second of this day - 23:59:59, but not as the first second
         --  of the next day - 00:00:00, so
         Sec_Of_Day := @ - 1;
      end if;

      Hour_Of_Check   := Sec_Of_Day / Seconds_In_Hour;
      Minute_Of_Check := (Sec_Of_Day rem Seconds_In_Hour) / 60;

      if Text_Report_ON then
         Report ("GNATCheck report");
         Report_EOL;

         Report_No_EOL ("date              : ");
         Report_No_EOL (Trim (Year (Time_Of_Check)'Img, Left) & '-');

         if Month_Of_Check < 10 then
            Report_No_EOL ("0");
         end if;

         Report_No_EOL (Trim (Month_Of_Check'Img, Left) & '-');

         if Day_Of_Check < 10 then
            Report_No_EOL ("0");
         end if;

         Report_No_EOL (Trim (Day_Of_Check'Img, Left) & ' ');

         if Hour_Of_Check < 10 then
            Report_No_EOL ("0");
         end if;

         Report_No_EOL (Trim (Hour_Of_Check'Img, Left) & ':');

         if Minute_Of_Check < 10 then
            Report_No_EOL ("0");
         end if;

         Report        (Trim (Minute_Of_Check'Img, Left));

         Report (Executable & " version : " & Version_String);

         Report_No_EOL ("command line      : ");
         Print_Gnatcheck_Command_Line;

         Report_No_EOL ("runtime           : ");
         Print_Runtime;
      end if;

      if XML_Report_ON then
         XML_Report_No_EOL ("<date>" &
                            Trim (Year (Time_Of_Check)'Img, Left) & '-',
                            Indent_Level => 1);
         XML_Report_No_EOL (if Month_Of_Check < 10 then "0" else "");
         XML_Report_No_EOL (Trim (Month_Of_Check'Img, Left) & '-');
         XML_Report_No_EOL (if Day_Of_Check < 10 then "0" else "");
         XML_Report_No_EOL (Trim (Day_Of_Check'Img, Left) & ' ');
         XML_Report_No_EOL (if Hour_Of_Check < 10 then "0" else "");
         XML_Report_No_EOL (Trim (Hour_Of_Check'Img, Left) & ':');
         XML_Report_No_EOL (if Minute_Of_Check < 10 then "0" else "");
         XML_Report        (Trim (Minute_Of_Check'Img, Left) & "</date>");

         XML_Report ("<version>gnatcheck " & Version_String & "</version>",
                     Indent_Level => 1);

         XML_Report_No_EOL ("<command-line>",
                            Indent_Level => 1);
         Print_Gnatcheck_Command_Line (XML => True);
         XML_Report        ("</command-line>");

         XML_Report_No_EOL ("<runtime>",
                            Indent_Level => 1);
         Print_Runtime (XML => True);
         XML_Report        ("</runtime>");
      end if;
   end Print_Report_Header;

   -------------------
   -- Print_Runtime --
   -------------------

   procedure Print_Runtime (XML : Boolean := False) is
   begin
      if RTS_Path.all /= "" then
         if XML then
            XML_Report (RTS_Path.all);
         else
            Report (RTS_Path.all);
         end if;
      else
         if XML then
            XML_Report_No_EOL ("default");
         else
            Report ("<default>");
         end if;
      end if;
   end Print_Runtime;

   -----------------------------
   -- Print_Violation_Summary --
   -----------------------------

   procedure Print_Violation_Summary is
   begin
      if Text_Report_ON then
         Report
           ("non-exempted violations               :" &
            Detected_Non_Exempted_Violations'Img, 1);
         Report
           ("rule exemption warnings               :" &
            Detected_Exemption_Warning'Img, 1);
         Report
           ("compilation errors                    :" &
           Detected_Compiler_Error'Img, 1);
         Report
           ("exempted violations                   :" &
            Detected_Exempted_Violations'Img, 1);
         Report
           ("internal errors                       :" &
            Detected_Internal_Error'Img, 1);
      end if;

      if XML_Report_ON then
         XML_Report ("<non-exempted-violations>"              &
                     Image (Detected_Non_Exempted_Violations) &
                     "</non-exempted-violations>",
                     Indent_Level => 1);
         XML_Report ("<rule-exemption-warnings>"     &
                     Image (Detected_Exemption_Warning) &
                     "</rule-exemption-warnings>",
                     Indent_Level => 1);
         XML_Report ("<compilation-errors>"     &
                     Image (Detected_Compiler_Error) &
                     "</compilation-errors>",
                     Indent_Level => 1);
         XML_Report ("<exempted-violations>"     &
                     Image (Detected_Exempted_Violations) &
                     "</exempted-violations>",
                     Indent_Level => 1);
         XML_Report ("<gnatcheck-errors>"     &
                     Image (Detected_Internal_Error) &
                     "</gnatcheck-errors>",
                     Indent_Level => 1);
         XML_Report ("</summary>");
      end if;
   end Print_Violation_Summary;

   -------------------------
   -- Is_Exemption_Pragma --
   -------------------------

   function Is_Exemption_Pragma
     (El : LAL.Analysis.Pragma_Node) return Boolean
   is
      Pragma_Name : constant Text_Type := To_Lower (El.F_Id.Text);
      Pragma_Args : constant LAL.Analysis.Base_Assoc_List := El.F_Args;
   begin
      return Pragma_Name in "annotate" | "gnat_annotate"
         and then not Pragma_Args.Is_Null
         and then To_Lower
           (Pragma_Args.List_Child (1).P_Assoc_Expr.Text) = "gnatcheck";
   end Is_Exemption_Pragma;

   ------------------------------
   -- Process_Exemption_Pragma --
   ------------------------------

   procedure Process_Exemption_Pragma (El : LAL.Analysis.Pragma_Node) is
      Pragma_Args : constant LAL.Analysis.Base_Assoc_List := El.F_Args;

      Next_Arg      : LAL.Analysis.Expr;
      Tmp_Str       : String_Access;
      First_Par_Idx : Natural;
      Pars_Start    : Natural;
      pragma Unreferenced (Pars_Start);
      Pars_End      : Natural;
      Last_Par_Idx  : Natural;
      Has_Param     : Boolean := False;
      Exem_Span     : Source_Location_Range := No_Source_Location_Range;
      SF            : constant SF_Id := File_Find (El.Unit.Get_Filename);

      Rule           : Rule_Id;
      Exemption_Kind : Exemption_Kinds;

      Exempted_At    : Parametrized_Exemption_Sections.Cursor;
      --  Used to check if the same rule with the same parameters is already
      --  exempted - set to No_Element if it is not the case or points to the
      --  record corresponding to the exemption section that exempts the same
      --  rule with the same parameters

      Par : String_Access;

      use Gnatcheck.Rules.Exemption_Parameters;

      use LCO;

   begin
      --  First, analyze the pragma format:
      --
      --  1. Check that we have at least three parameters

      if Pragma_Args.Children_Count < 3 then
         Store_Diagnosis
           (Full_File_Name     => El.Unit.Get_Filename,
            Sloc               => Start_Sloc (El.Sloc_Range),
            Message            => "too few parameters for exemption, ignored",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
         return;
      end if;

      --  2. Second parameter should be either "Exempt_On" or "Exempt_Off"

      Next_Arg := Pragma_Args.List_Child (2).P_Assoc_Expr;
      Exemption_Kind := Get_Exemption_Kind (Next_Arg.Text);

      if Exemption_Kind = Not_An_Exemption then
         Store_Diagnosis
           (Full_File_Name     => El.Unit.Get_Filename,
            Sloc               => Start_Sloc (El.Sloc_Range),
            Message            => "wrong exemption kind, ignored",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
         return;
      end if;

      --  3. Third parameter should be the name of some existing rule, may be,
      --     with parameter names, but the latter is allowed only if fine-tuned
      --     exemptions is allowed for the rule, and if parameter names make
      --     sense for the given rule:

      Next_Arg := Pragma_Args.List_Child (3).P_Assoc_Expr;

      if Next_Arg.Kind = LCO.Ada_String_Literal then
         Tmp_Str   := new String'(Image (Next_Arg.Text));
         First_Par_Idx := Tmp_Str'First + 1;  --  skip '"'
         Last_Par_Idx  := Index (Tmp_Str.all, ":");

         if Last_Par_Idx = 0 then
            Last_Par_Idx := Tmp_Str'Last - 1; --  skip '"'
         else
            Last_Par_Idx := Last_Par_Idx - 1; --  skip ':'
            Has_Param    := True;
         end if;

         Rule := Get_Rule (Trim (Tmp_Str (First_Par_Idx .. Last_Par_Idx),
                                 Both));
      else
         Rule := No_Rule;
      end if;

      --  3.1 Check if we have an existing rule

      if not Present (Rule) then
         Store_Diagnosis
           (Full_File_Name     => El.Unit.Get_Filename,
            Sloc               => Start_Sloc (El.Sloc_Range),
            Message            => "wrong rule name in exemption, ignored",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
         Free (Tmp_Str);
         return;
      end if;

      --  3.2 Check if we have parameter(s) specified then parameter(s) make(s)
      --      sense for the rule.

      if Has_Param then
         if not Allows_Parametrized_Exemption (Rule) then
            Store_Diagnosis
              (Full_File_Name     => El.Unit.Get_Filename,
               Sloc               => Start_Sloc (El.Sloc_Range),
               Message            => "rule " & Rule_Name (Rule) &
                                     " cannot have parametric " &
                                     "exemption, ignored",
               Diagnosis_Kind     => Exemption_Warning,
               SF                 => SF);
               Free (Tmp_Str);
            return;
         end if;

         First_Par_Idx := Last_Par_Idx + 2;
         Pars_End := Tmp_Str'Last - 1;

         while First_Par_Idx < Pars_End
           and then Tmp_Str (First_Par_Idx) = ' '
         loop
            First_Par_Idx := First_Par_Idx + 1;
         end loop;

         Set_Rule_Exempt_Pars
           (Rule,
            Tmp_Str (First_Par_Idx .. Pars_End),
            SF,
            Sloc_Image (Start_Sloc (El.Sloc_Range)));

         if Is_Empty (Rule_Exemption_Parameters) then
            Free (Tmp_Str);
            return;
         end if;
      end if;

      --  4. Fourth parameter, if present, should be a string.

      if Pragma_Args.Children_Count >= 4 then
         Next_Arg := Pragma_Args.List_Child (4).P_Assoc_Expr;

         --  Evaluate the static string expression of the fourth parameter via
         --  Libadalang.Expr_Eval.
         begin
            declare
               use Libadalang.Expr_Eval;
               Eval_Res : constant Eval_Result :=
                 Expr_Eval (Pragma_Args.List_Child (4).P_Assoc_Expr);
            begin
               Tmp_Str :=
                 new String'(Image (To_Text (As_String (Eval_Res))));
            end;
         exception
            when Property_Error =>
               --  If we couldn't evaluate the expression as a string, a
               --  property_error will have been raised. In that case, emit a
               --  diagnosis.
               Store_Diagnosis
                 (Full_File_Name     => El.Unit.Get_Filename,
                  Sloc               => Start_Sloc (El.Sloc_Range),
                  Message            =>
                    "exemption justification should be a string",
                  Diagnosis_Kind     => Exemption_Warning,
                  SF                 => SF);
         end;

         --  5. Fourth parameter is ignored if exemption is turned OFF

         if Exemption_Kind = Exempt_Off then
            Store_Diagnosis
              (Full_File_Name     => El.Unit.Get_Filename,
               Sloc               => Start_Sloc (El.Sloc_Range),
               Message            =>
                 "turning exemption OFF does not need justification",
               Diagnosis_Kind     => Exemption_Warning,
               SF                 => SF);
         end if;
      end if;

      --  6. If exemption is turned ON, justification is expected

      if Exemption_Kind = Exempt_On and then Pragma_Args.Children_Count = 3
      then
         Store_Diagnosis
           (Full_File_Name     => El.Unit.Get_Filename,
            Sloc               => Start_Sloc (El.Sloc_Range),
            Message            =>
              "turning exemption ON expects justification",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
      end if;

      if Pragma_Args.Children_Count > 4 then
         Next_Arg := Pragma_Args.List_Child (5).P_Assoc_Expr;
         Store_Diagnosis
           (Full_File_Name => El.Unit.Get_Filename,
            Sloc           => Start_Sloc (El.Sloc_Range),
            Message        =>
              "rule exemption may have at most 4 parameters",
            Diagnosis_Kind => Exemption_Warning,
            SF             => SF);
      end if;

      --  If Rule does not denote the enabled rule - nothing to do

      if not (Is_Enabled (Rule)
              or else
                (Rule = Warnings_Id and then Is_Enabled (Restrictions_Id)))
      then
       --  In case when a Restriction rule is enabled, we may want to use
       --  exemptions section for Warnings rule to suppress default warnings.
       --  We may get rid of this if and when we get a possibility to turn
       --  off all the warnings except related to restrictions only.
         return;
      end if;

      --  Now - processing of the exemption pragma. If we are here, we are
      --  sure that Rule denotes and existing and enabled rule.

      Exem_Span := El.Sloc_Range;

      case Exemption_Kind is
         when Exempt_On =>

            if Tmp_Str = null then
               Tmp_Str := new String'("unjustified");
            end if;

            if Is_Exempted (Rule) then
               Store_Diagnosis
                 (Full_File_Name     => El.Unit.Get_Filename,
                  Sloc               => Start_Sloc (El.Sloc_Range),
                  Message            =>
                    "rule " & Rule_Name (Rule)
                    & " is already exempted at line" &
                    Exemption_Sections (Rule).Line_Start'Img,
                  Diagnosis_Kind     => Exemption_Warning,
                  SF                 => SF);
               Free (Tmp_Str);
               return;
            end if;

            if not Has_Param and then Allows_Parametrized_Exemption (Rule)
            then
               --  Is Rule already exempted with parameters?
               if not Parametrized_Exemption_Sections.Is_Empty
                 (Rule_Param_Exempt_Sections (Rule))
               then
                  Store_Diagnosis
                    (Full_File_Name     => El.Unit.Get_Filename,
                     Sloc               => Start_Sloc (El.Sloc_Range),
                     Message            =>
                       "rule " & Rule_Name (Rule)
                       & " is already exempted with parameter(s) at line"
                       & Parametrized_Exemption_Sections.Element
                         (Parametrized_Exemption_Sections.First
                           (Rule_Param_Exempt_Sections (Rule))).
                             Exempt_Info.Line_Start'Img,
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  Free (Tmp_Str);
                  return;
               end if;
            end if;

            if Has_Param then
               Is_Exempted_With_Pars (Rule, Exempted_At);

               if Parametrized_Exemption_Sections.Has_Element (Exempted_At)
               then
                  Store_Diagnosis
                    (Full_File_Name     => El.Unit.Get_Filename,
                     Sloc               => Start_Sloc (El.Sloc_Range),
                     Message            =>
                       "rule " & Rule_Name (Rule) &
                       " is already exempted with the same parameters at line"
                       & Parametrized_Exemption_Sections.Element
                          (Exempted_At).Exempt_Info.Line_Start'Img,
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  Free (Tmp_Str);
                  return;
               else
                  --  We have to check the following:
                  --
                  --     Exempt_On, Rule:Par1, Par3
                  --     ...
                  --     Exempt_On, Rule:Par1, Par2

                  Same_Parameter_Exempted (Rule, Exempted_At, Par);

                  if Parametrized_Exemption_Sections.Has_Element (Exempted_At)
                  then
                     Store_Diagnosis
                       (Full_File_Name     => El.Unit.Get_Filename,
                        Sloc               => Start_Sloc (El.Sloc_Range),
                        Message            =>
                          "rule " & Rule_Name (Rule) &
                          " is already exempted with parameter '" &
                          Par.all & "' at line"                   &
                          Parametrized_Exemption_Sections.Element
                             (Exempted_At).Exempt_Info.Line_Start'Img,
                        Diagnosis_Kind     => Exemption_Warning,
                        SF                 => SF);

                     Free (Tmp_Str);
                     Free (Par);
                     return;
                  end if;

                --  If we are here then we know for sure that the
                --  parametric exemption is correct, and there is no
                --  open exemption section for this rule and this
                --  parameter(s). So we can just add the corresponding record
                --  to Rule_Param_Exempt_Sections:

                  Parametrized_Exemption_Sections.Insert
                    (Rule_Param_Exempt_Sections (Rule),
                    (Exempt_Info =>
                       (Line_Start    => Natural (Exem_Span.Start_Line),
                        Col_Start     => Natural (Exem_Span.Start_Column),
                        Line_End      => 0,
                        Col_End       => 0,
                        Justification => new
                          String'((Tmp_Str
                                   (Tmp_Str'First .. Tmp_Str'Last))),
                        Detected      => 0),
                     Rule        => Rule,
                     SF          => SF,
                   Params      => To_Pars_List (Rule_Exemption_Parameters)));
               end if;
            else
               Exemption_Sections (Rule) :=
                 (Line_Start    => Natural (Exem_Span.Start_Line),
                  Col_Start     => Natural (Exem_Span.Start_Column),
                  Line_End      => 0,
                  Col_End       => 0,
                  Justification =>
                    new String'((Tmp_Str
                                   (Tmp_Str'First .. Tmp_Str'Last))),
                  Detected      => 0);
            end if;

            Free (Tmp_Str);

         when Exempt_Off =>
            if Has_Param then
               Is_Exempted_With_Pars (Rule, Exempted_At);

               if not Parametrized_Exemption_Sections.Has_Element
                        (Exempted_At)
               then
                  Store_Diagnosis
                    (Full_File_Name     => El.Unit.Get_Filename,
                     Sloc               => Start_Sloc (El.Sloc_Range),
                     Message            =>
                       "rule "
                       & Rule_Name (Rule) & " is not in exempted state",
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  return;
               else
                  Turn_Off_Parametrized_Exemption
                    (Rule, Exempted_At, Exem_Span, SF);
               end if;
            else
               if not Is_Exempted (Rule) then
                  Store_Diagnosis
                    (Full_File_Name     => El.Unit.Get_Filename,
                     Sloc               => Start_Sloc (El.Sloc_Range),
                     Message            =>
                       "rule "
                       & Rule_Name (Rule) & " is not in exempted state",
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  return;
               end if;

               Turn_Off_Exemption (Rule, Exem_Span, SF);
            end if;

         when Not_An_Exemption =>
            pragma Assert (False);
      end case;
   end Process_Exemption_Pragma;

   ----------------------------------
   -- Process_Postponed_Exemptions --
   ----------------------------------

   procedure Process_Postponed_Exemptions is
      Next_Postponed_Section  : Postponed_Rule_Exemption_Info_Access;
      Next_Post_Param_Section : Parametrized_Exemption_Sections.Cursor;
      use Parametrized_Exemption_Sections;

      Next_Par_S_Info : Parametrized_Exemption_Info;

      procedure Map_Diagnosis (Position : Error_Messages_Storage.Cursor);
      --  Maps the diagnosis pointed by the argument onto stored information
      --  about exemption sections. If the diagnosis points to some place
      --  inside some exemption section, and the diagnosis is not exempted,
      --  then the diagnosis is exempted by adding the justification from the
      --  exemption section, and the corresponding exemption violation is
      --  counted for the given exemption section

      procedure Map_Diagnosis (Position : Error_Messages_Storage.Cursor) is
         Diag : Diag_Message := Error_Messages_Storage.Element (Position);
         Diag_Line   : Positive;
         Diag_Column : Positive;
         SF          : SF_Id;
         Is_Exempted : Boolean;
      begin
         if Diag.Diagnosis_Kind /= Rule_Violation then
            return;
         end if;

         if Diag.Justification /= null then
            --  some diagnoses may be already exempted
            return;
         end if;

         SF := Diag.SF;

         if not Present (SF) then
            --  This is the case when the diagnosis is generated for
            --  expanded generic, and the generic itself is not a
            --  gnatcheck input.

            return;
         end if;

         --  First, check for non-parametric exemptions:

         Diag_Line := Select_Line (Diag.Text);

         Map_On_Postponed_Check_Exemption
           (In_File       => SF,
            For_Check     => Diag.Rule,
            For_Line      => Diag_Line,
            Is_Exempted   => Is_Exempted,
            Justification => Diag.Justification);

         --  Check for parametric exemptions

         if not Is_Exempted
           and then Allows_Parametrized_Exemption (Diag.Rule)
           and then not Parametrized_Exemption_Sections.Is_Empty
                          (Postponed_Param_Exempt_Sections (Diag.Rule) (SF))
         then
            Diag_Column := Select_Column (Diag.Text);
            Diag.Justification :=
              Get_Param_Justification
                (Rule => Diag.Rule,
                 Diag => Diag.Text.all,
                 SF   => SF,
                 Line => Diag_Line,
                 Col  => Diag_Column);
         end if;

         if Diag.Justification /= null then
            Error_Messages_Storage.Replace_Element
              (Container => All_Error_Messages,
               Position  => Position,
               New_Item  => Diag);
         end if;
      end Map_Diagnosis;

   --  Start of processing for Process_Postponed_Exemptions

   begin
      Error_Messages_Storage.Iterate
        (Container => All_Error_Messages,
         Process   => Map_Diagnosis'Access);

      --  Now, iterate through the stored exemption and generate exemption
      --  warnings for those of them for which no exempted diagnoses are found.

      for Rule in First_Compiler_Check .. All_Rules.Last loop
         for SF in First_SF_Id .. Last_Argument_Source loop

            --  Non-parametric exemptions:
            Next_Postponed_Section :=
              Postponed_Exemption_Sections (Rule) (SF);

            while Next_Postponed_Section /= null loop
               if Next_Postponed_Section.Exemption_Section.Detected = 0 then
                  Store_Diagnosis
                    (Text =>
                       File_Name (SF) & ':' &
                       Sloc_Image (Next_Postponed_Section.Exemption_Section.
                                   Line_End,
                                   Next_Postponed_Section.Exemption_Section.
                                   Col_End) &
                       ": no detection for " & Rule_Name (Rule) &
                       " rule in exemption section starting at line" &
                       Next_Postponed_Section.Exemption_Section.
                       Line_Start'Img,
                     Diagnosis_Kind => Exemption_Warning,
                     SF             => SF);
               end if;

               Next_Postponed_Section :=
                 Next_Postponed_Section.Next_Exemption_Section;
            end loop;

         --  Parametric exemptions:
            if Allows_Parametrized_Exemption (Rule)
              and then
               not Is_Empty (Postponed_Param_Exempt_Sections (Rule) (SF))
            then
               Next_Post_Param_Section :=
                 First (Postponed_Param_Exempt_Sections (Rule) (SF));

               while Has_Element (Next_Post_Param_Section) loop
                  Next_Par_S_Info :=
                    Parametrized_Exemption_Sections.Element
                      (Next_Post_Param_Section);

                  if Next_Par_S_Info.Exempt_Info.Detected = 0 then
                     Store_Diagnosis
                       (Text =>
                          File_Name (SF) & ':' &
                          Sloc_Image (Next_Par_S_Info.Exempt_Info.Line_End,
                                      Next_Par_S_Info.Exempt_Info.Col_End) &
                          ": no detection for '" & Rule_Name (Rule) & ": " &
                          Params_Img (Next_Par_S_Info.Params, Rule) &
                          "' rule in exemption section starting "   &
                          "at line" &
                          Next_Par_S_Info.Exempt_Info.Line_Start'Img,
                        Diagnosis_Kind => Exemption_Warning,
                        SF             => SF);
                  end if;

                  Next_Post_Param_Section := Next (Next_Post_Param_Section);
               end loop;
            end if;
         end loop;
      end loop;
   end Process_Postponed_Exemptions;

   ---------------------------
   -- Process_User_Filename --
   ---------------------------

   procedure Process_User_Filename (Fname : String) is
   begin
      if Is_Regular_File (Fname) then
         if User_Info_File /= null then
            Error ("--include-file option can be given only once, " &
                   "all but first ignored");
         else
            User_Info_File           := new String'(Fname);
            User_Info_File_Full_Path := new String'
              (Normalize_Pathname
                 (Fname,
                  Resolve_Links  => False,
                  Case_Sensitive => False));
         end if;
      else
         Error (Fname & " not found, --include-file option ignored");
      end if;
   end Process_User_Filename;

   --------------------
   -- Rule_Parameter --
   --------------------

   function Rule_Parameter (Diag : String; Rule : Rule_Id) return String is
   begin
      case Rule is
         when Restrictions_Id =>
            return Restriction_Rule_Parameter (Diag);
         when Style_Checks_Id =>
            --  ??? would need a -gnaty switch similar to -gnatw.d
            return "";
         when Warnings_Id     =>
            return Warning_Rule_Parameter (Diag);
         when others =>
            return Rule_Parameter (All_Rules.Table (Rule).all, Diag);
      end case;
   end Rule_Parameter;

   -------------------------------------------------
   -- Rule_Parametrized_Exem_Sections_Debug_Image --
   -------------------------------------------------

   procedure Rule_Parametrized_Exem_Sections_Debug_Image (Rule : Rule_Id) is
   begin
      Info ("** PARAMETRIC EXEMPTION SECTIONS FOR RULE" & Rule'Img &
            " (" & Rule_Name (Rule) & ") **");

      if Parametrized_Exemption_Sections.Is_Empty
        (Rule_Param_Exempt_Sections (Rule))
      then
         Info ("   No parametric exemption sections");
      else
         Parametrized_Exemption_Sections.Iterate
           (Container => Rule_Param_Exempt_Sections (Rule),
            Process   => Parametrized_Exem_Section_Debug_Image'Access);
      end if;
   end Rule_Parametrized_Exem_Sections_Debug_Image;

   -----------------------------
   -- Same_Parameter_Exempted --
   -----------------------------

   procedure Same_Parameter_Exempted
     (Rule        :     Rule_Id;
      Exempted_At : out Parametrized_Exemption_Sections.Cursor;
      Par         : out String_Access)
   is
      Next_Par : Exemption_Parameters.Cursor :=
        Exemption_Parameters.First (Rule_Exemption_Parameters);

      use Parametrized_Exemption_Sections;
      Next_Section : Cursor;

   begin
      Free (Par);

      Exempted_At := No_Element;

      if not Is_Empty (Rule_Param_Exempt_Sections (Rule)) then
         while Exemption_Parameters.Has_Element (Next_Par) loop
            Next_Section := First (Rule_Param_Exempt_Sections (Rule));

            while Has_Element (Next_Section) loop
               if Belongs
                    (Exemption_Parameters.Element (Next_Par),
                     Parametrized_Exemption_Sections.Element
                       (Next_Section).Params)
               then
                  Exempted_At := Next_Section;
                  Par         :=
                    new String'(Exemption_Parameters.Element (Next_Par));
                  return;
               end if;

               Next_Section := Next (Next_Section);
            end loop;

            Next_Par := Exemption_Parameters.Next (Next_Par);
         end loop;
      end if;
   end Same_Parameter_Exempted;

   -------------------
   -- Select_Column --
   -------------------

   function Select_Column (Diag : String_Access) return Positive is
      Start_Idx : Natural;
      End_Idx   : Natural;
   begin
      Start_Idx := Index (Diag.all, ":") + 1;
      Start_Idx := Index (Diag (Start_Idx .. Diag'Last), ":") + 1;
      End_Idx   := Index (Diag (Start_Idx .. Diag'Last), ":") - 1;

      return Positive'Value (Diag (Start_Idx .. End_Idx));
   end Select_Column;

   -----------------
   -- Select_Line --
   -----------------

   function Select_Line (Diag : String_Access) return Positive is
      Start_Idx : Natural;
      End_Idx   : Natural;
   begin
      Start_Idx := Index (Diag.all, ":");

      if Start_Idx = Diag'First + 1
        and then Diag (Start_Idx + 1) = '\'
      then
         Start_Idx := Index (Diag (Start_Idx + 2 .. Diag'Last), ":");
      end if;

      Start_Idx := @ + 1;
      End_Idx   := Index (Diag (Start_Idx .. Diag'Last), ":") - 1;
      return Positive'Value (Diag (Start_Idx .. End_Idx));
   end Select_Line;

   --------------------------
   -- Set_Rule_Exempt_Pars --
   --------------------------

   procedure Set_Rule_Exempt_Pars
     (Rule : Rule_Id;
      Pars : String;
      SF   : SF_Id;
      SLOC : String)
   is
      use Gnatcheck.Rules.Exemption_Parameters;

      Par_Start   :          Natural := Pars'First;
      Par_End     :          Natural;
      Last_Idx    : constant Natural    := Pars'Last;
      Position    :          Cursor;
      Success     :          Boolean;
      Warning_Par : constant Boolean := Rule = Warnings_Id;
      --  In case of Warnings rule, we consider parameters one by one. That is,
      --  for "ad.c.d.fg" as Pars string we separately store 'a', 'd', '.c',
      --  '.d', '.f' and 'g'

   begin
      Clear (Rule_Exemption_Parameters);

      loop
         while Par_Start <= Last_Idx and then Pars (Par_Start) = ' ' loop
            Par_Start := @ + 1;
         end loop;

         exit when Par_Start > Last_Idx;

         if Warning_Par then
            Par_End :=
              (if Pars (Par_Start) = '.' then Par_Start + 1 else Par_Start);
         else
            Par_End := Index (Pars (Par_Start + 1 .. Last_Idx), [',']);
         end if;

         if Par_End = 0 then
            Par_End := Last_Idx;
         elsif not Warning_Par then
            Par_End := Par_End - 1;
         end if;

         while Pars (Par_End) = ' ' loop
            Par_End := Par_End - 1;
         end loop;

         if Allowed_As_Exemption_Parameter
              (Rule,
               (if Rule = Warnings_Id
                then Remove_Spaces (Pars (Par_Start .. Par_End))
                else To_Lower (Remove_Spaces (Pars (Par_Start .. Par_End)))))
         then
            Insert (Container => Rule_Exemption_Parameters,
                    New_Item  =>
                      (if Rule = Warnings_Id then
                          Remove_Spaces (Pars (Par_Start .. Par_End))
                       else
                          To_Lower (Remove_Spaces
                                      (Pars (Par_Start .. Par_End)))),
                    Position  => Position,
                    Inserted  => Success);

            if not Success then
               Store_Diagnosis
                 (Text           => File_Name (SF) & ":" & SLOC &
                                    ": parameter " &
                                    Pars (Par_Start .. Par_End) &
                                    " duplicated in rule exemption",
                  Diagnosis_Kind => Exemption_Warning,
                  SF             => SF);
            end if;

         else
            Store_Diagnosis
              (Text           => File_Name (SF) & ":" & SLOC &
                                 ": parameter " &
                                 Pars (Par_Start .. Par_End) &
                                 " is not allowed in exemption for rule " &
                                 Rule_Name (Rule),
               Diagnosis_Kind => Exemption_Warning,
               SF             => SF);

            Clear (Rule_Exemption_Parameters);
         end if;

         if Warning_Par then
            Par_Start := @ + 1;
            exit when Par_Start > Last_Idx;
         else
            Par_Start := Index (Pars (Par_End + 1 .. Last_Idx), ",");

            if Par_Start = 0 then
               exit;
            else
               Par_Start := @ + 1;
            end if;
         end if;
      end loop;
   end Set_Rule_Exempt_Pars;

   ----------------
   -- Sloc_Image --
   ----------------

   function Sloc_Image (Line, Column : Natural) return String is
      Line_Image : constant String := Line'Image;
      Col_Image  : constant String := Column'Image;
   begin
      if Column < 10 then
         return Line_Image (2 .. Line_Image'Last) & ":0" &
                Col_Image (2 .. Col_Image'Last);
      else
         return Line_Image (2 .. Line_Image'Last) & ":" &
                Col_Image (2 .. Col_Image'Last);
      end if;
   end Sloc_Image;

   function Sloc_Image (Sloc : Source_Location) return String is
   begin
      return Sloc_Image (Natural (Sloc.Line), Natural (Sloc.Column));
   end Sloc_Image;

   ---------------------
   -- Store_Diagnosis --
   ---------------------

   procedure Store_Diagnosis
     (Full_File_Name : String;
      Message        : String;
      Sloc           : Source_Location;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id       := No_Rule;
      Justification  : String_Access := null)
   is
      use Ada.Directories;

   begin
      Store_Diagnosis
        (Text           =>
           (if Full_Source_Locations
            then Full_File_Name
            else Simple_Name (Full_File_Name)) & ":" &
           Sloc_Image (Sloc) & ": " &
           Message
           & (if Rule /= No_Rule
              then Annotate_Rule (All_Rules.Table (Rule).all)
              else ""),
         Diagnosis_Kind => Diagnosis_Kind,
         SF             => SF,
         Rule           => Rule,
         Justification  => Justification);
   end Store_Diagnosis;

   procedure Store_Diagnosis
     (Text           : String;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id        := No_Rule;
      Justification  : String_Access  := null)
   is
      Tmp : Diag_Message :=
        (Text           => new String'(Text),
         Justification  => Justification,
         Diagnosis_Kind => Diagnosis_Kind,
         Rule           => Rule,
         SF             => SF);

   begin
      --  We need this check to avoid diagnoses duplication. Our set container
      --  has broken "<" relation, so Insert may add diagnoses that are already
      --  stored in the container (see the documentation for "<" for more
      --  details.

      if not Error_Messages_Storage.Contains
               (Container => All_Error_Messages,
                Item      => Tmp)
      then
         if Diagnosis_Kind = Compiler_Error then
            Set_Source_Status (SF, Not_A_Legal_Source);
         elsif Diagnosis_Kind = Internal_Error then
            Set_Source_Status (SF, Error_Detected);
         end if;

         if Justification /= null then
            --  Here we count detections for non-parametric exemption
            --  sections only

            Exemption_Sections (Rule).Detected := @ + 1;
         end if;

         Error_Messages_Storage.Insert
           (Container => All_Error_Messages,
            New_Item  => Tmp,
            Position  => Unused_Position,
            Inserted  => Unused_Inserted);

      else
         Free (Tmp.Text);
      end if;
   end Store_Diagnosis;

   -------------------
   -- Strip_Warning --
   -------------------

   function Strip_Warning (Diag : String) return String is
      Idx : constant Natural := Index (Diag, " [-gnatw");
   begin
      return (if Idx = 0 then Diag else Diag (Diag'First .. Idx - 1));
   end Strip_Warning;

   ------------------
   -- To_Pars_List --
   ------------------

   function To_Pars_List
     (Pars : Exemption_Parameters.Set)
      return String_List_Access
   is
      use Exemption_Parameters;

      Result : constant String_List_Access := new
        String_List (1 .. Natural (Length (Pars)));
      Next_Par : Cursor := First (Pars);
   begin
      for J in Result'Range loop
         Result (J) :=
           new String'(Exemption_Parameters.Element (Next_Par));
         Next_Par := Next (Next_Par);
      end loop;

      return Result;
   end To_Pars_List;

   ------------------------
   -- Turn_Off_Exemption --
   ------------------------

   procedure Turn_Off_Exemption
     (Rule         : Rule_Id;
      Closing_Span : Source_Location_Range;
      SF           : SF_Id)
   is
      Tmp : Postponed_Rule_Exemption_Info_Access;
   begin
      Exemption_Sections (Rule).Line_End := Natural (Closing_Span.End_Line);
      Exemption_Sections (Rule).Col_End  :=
        Natural (Closing_Span.End_Column);

      Tmp := new Postponed_Rule_Exemption_Info;
      Tmp.Exemption_Section := Exemption_Sections (Rule);
      Tmp.Next_Exemption_Section := Postponed_Exemption_Sections (Rule) (SF);
      Postponed_Exemption_Sections (Rule) (SF) := Tmp;

      Exemption_Sections (Rule) :=
        (Line_Start    => 0,
         Col_Start     => 0,
         Line_End      => 0,
         Col_End       => 0,
         Justification => null,
         Detected      => 0);
   end Turn_Off_Exemption;

   -------------------------------------
   -- Turn_Off_Parametrized_Exemption --
   -------------------------------------

   procedure Turn_Off_Parametrized_Exemption
     (Rule         : Rule_Id;
      Exempted_At  : in out Parametrized_Exemption_Sections.Cursor;
      Closing_Span : Source_Location_Range;
      SF           : SF_Id)
   is
      New_Section : Parametrized_Exemption_Info;
   begin
      New_Section := Parametrized_Exemption_Sections.Element (Exempted_At);
      New_Section.Exempt_Info.Line_End := Natural (Closing_Span.End_Line);
      New_Section.Exempt_Info.Col_End  := Natural (Closing_Span.End_Column);

      Parametrized_Exemption_Sections.Insert
        (Container => Postponed_Param_Exempt_Sections (Rule) (SF),
         New_Item  => New_Section);
      Parametrized_Exemption_Sections.Delete
        (Rule_Param_Exempt_Sections (Rule), Exempted_At);
   end Turn_Off_Parametrized_Exemption;

   --------------------------
   -- XML_Report_Diagnosis --
   --------------------------

   procedure XML_Report_Diagnosis
     (Diag         : Diag_Message;
      Short_Report : Boolean)
   is
      Indentation : constant Natural := (if Short_Report then 1 else 2);
      Exempted    : constant Boolean :=
        Diag.Diagnosis_Kind = Rule_Violation and then
        Diag.Justification /= null;
      Message     : constant String  := Strip_Warning (Diag.Text.all);
      L_Idx       :          Natural := Message'First;
      R_Idx       :          Natural := Index (Message, ":");
      Last_Idx    : constant Natural := Message'Last;

   begin
      --  Correct R_Idx in case of Windows-style full file name
      --  (starts from <dik_letter>:\ )

      if R_Idx = L_Idx + 1
        and then
         Message (R_Idx + 1) = '\'
      then
         R_Idx := Index (Message (R_Idx + 1 .. Last_Idx), ":");
      end if;

      XML_Report_No_EOL
        ((if Diag.Diagnosis_Kind = Exemption_Warning then
             "<exemption-problem"
          elsif Diag.Diagnosis_Kind = Compiler_Error then
             "<compiler-error"
          elsif Diag.Diagnosis_Kind = Internal_Error then
             "<internal-error"
          elsif Exempted then
             "<exempted-violation"
          else "<violation"),
         Indent_Level => Indentation);

      XML_Report_No_EOL
        (" file=" & """" & Escape_XML (Message (L_Idx .. R_Idx - 1)) & """ ");

      L_Idx := R_Idx + 1;
      R_Idx := Index (Message (L_Idx .. Last_Idx), ":");

      XML_Report_No_EOL
        ("line=" & """" & Message (L_Idx .. R_Idx - 1) & """ ");

      L_Idx := R_Idx + 1;
      R_Idx := Index (Message (L_Idx .. Last_Idx), ":");

      while Message (L_Idx) = '0' loop
         L_Idx := @ + 1;
      end loop;

      XML_Report_No_EOL
        ("column=" & """" & Message (L_Idx .. R_Idx - 1) & """");

      if Diag.Diagnosis_Kind = Rule_Violation then
         XML_Report_No_EOL (" rule-id=""" & Rule_Name (Diag.Rule) & '"');
      end if;

      XML_Report (">");
      XML_Report
        ("<message>" & Escape_XML (Message (R_Idx + 2 .. Last_Idx)) &
         "</message>",
         Indent_Level => Indentation + 1);

      if Exempted then
         XML_Report
           ("<justification>" & Escape_XML (Diag.Justification.all) &
            "</justification>",
            Indent_Level => Indentation + 1);
      end if;

      XML_Report
        ((if Diag.Diagnosis_Kind = Exemption_Warning then
             "</exemption-problem>"
          elsif Diag.Diagnosis_Kind = Compiler_Error then
             "</compiler-error>"
          elsif Diag.Diagnosis_Kind = Internal_Error then
             "</internal-error>"
          elsif Exempted then
             "</exempted-violation>"
          else "</violation>"),
         Indent_Level => Indentation);
   end XML_Report_Diagnosis;

end Gnatcheck.Diagnoses;
