--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Case_Util;
with GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;

with Gnatcheck.Compiler;           use Gnatcheck.Compiler;
with Gnatcheck.Options;            use Gnatcheck.Options;
with Gnatcheck.Output;             use Gnatcheck.Output;
with Gnatcheck.Projects.Aggregate; use Gnatcheck.Projects.Aggregate;
with Gnatcheck.Rules;              use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table;   use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.String_Utilities;   use Gnatcheck.String_Utilities;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Expr_Eval;

package body Gnatcheck.Diagnoses is

   use all type Ada.Containers.Count_Type;

   package LCO renames Libadalang.Common;

   Match_Diagnosis : constant Pattern_Matcher :=
     Compile ("^(([A-Z]:)?[^:]*):(\d+):(\d+): (.*)$");
   --  Matcher for a diagnostic

   Match_Rule_Name : constant Pattern_Matcher :=
     Compile ("^""(\w+)\s*(?::\s*(.*))?""$");
   --  Matcher for a rule name and potential arguments

   Match_Rule_Param : constant Pattern_Matcher :=
     Compile ("([^,]+)?\s*,?\s*");

   Match_Rule_Warning_Param : constant Pattern_Matcher :=
     Compile ("(\.?\w)");

   Match_Exempt_Comment : constant Pattern_Matcher :=
     Compile ("--##\s*rule\s+(line\s+)?(on|off)\s+([^\s]+)[^#]*(?:##(.*))?");

   -----------------------
   -- Diagnoses storage --
   -----------------------

   type Diag_Message is record
      File           : Unbounded_String;
      Sloc           : Langkit_Support.Slocs.Source_Location;
      Text           : Unbounded_String;
      Justification  : Unbounded_String;
      Diagnosis_Kind : Diagnosis_Kinds;
      Rule           : Rule_Id;
      SF             : SF_Id;
   end record;

   function "<" (L, R : Diag_Message) return Boolean;

   function Image (Self : Diag_Message) return String;

   package Error_Messages_Storage is new Ada.Containers.Ordered_Sets
     (Element_Type => Diag_Message, "=" => "=", "<" => "<");

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

   function Strip_Tag (Diag : String) return String;
   --  Strip trailing " [-gnatxxx]", if any

   ----------------------------------------------------------------------
   --  Data structures and local routines for rule exemption mechanism --
   ----------------------------------------------------------------------

   type Exemption_Kinds is (Not_An_Exemption, Exempt_On, Exempt_Off);

   function Get_Exemption_Kind
     (Image : Wide_Wide_String) return Exemption_Kinds;
   --  Returns Exemption_Kinds value represented by Image. Returns
   --  Not_An_Exemption if Image does not represent a valid exemption kind.

   -------------------
   -- Exempt_Action --
   -------------------

   type Exempt_Action is record
      Exemption_Control   : Exemption_Kinds;
      Rule_Name           : Unbounded_String := Null_Unbounded_String;
      Params              : Rule_Params;
      Justification       : Unbounded_String := Null_Unbounded_String;
      Check_Justification : Boolean := True;
      Sloc_Range          : Langkit_Support.Slocs.Source_Location_Range;
      Unit                : LAL.Analysis.Analysis_Unit;
   end record;
   --  Stores information about an exemption action (either triggered by a
   --  ``pragma Annotate``, or by a ``--#`` comment).
   --
   --  This type and the associated primitive are used to decouple the
   --  validation and processing of the action, and are used both by comment
   --  based exemptions and pragma based ones.

   procedure Process_Exempt_Action (Self : Exempt_Action);
   --  Process the given exempt action, doing some legality checks on the
   --  Exempt_Action, and creating the necessary exemption information in the
   --  sections arrays.

   type Exemption_Info is record
      Line_Start : Natural;
      Col_Start  : Natural;
      --  Location of exemption pragma that turns exemption ON

      Line_End : Natural;
      Col_End  : Natural;
      --  End of the exemption section

      Justification : Unbounded_String;

      Detected : Natural;
      --  Number of the diagnoses generated for exempted rule
   end record;

   package Exemption_Sections_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Rule_Id,
      Element_Type    => Exemption_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");
   Exemption_Sections : Exemption_Sections_Map.Map;
   --  A map to store all exemption sections, mapped from their rule
   --  identifier.

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
     (Rule : Rule_Id; Closing_Sloc : Source_Location; SF : SF_Id);
   --  Cleans up the stored exemption section for the argument Rule.

   -----------------------------
   -- Parametric exemptions --
   -----------------------------

   type Parametrized_Exemption_Info is record
      Exempt_Info : Exemption_Info;
      Rule        : Rule_Id;
      SF          : SF_Id;
      Params      : Rule_Params;
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

   function Params_Img (Params : Rule_Params; Rule : Rule_Id) return String;
   --  Returns the string image of rule exemption parameters that are supposed
   --  to be stored in Params list (and Params is supposed to be not null) in
   --  the format suitable for including in a diagnosis. Rule parameter is
   --  needed to decide if we should fold the parameter in proper case (for
   --  sure we should not do this for Warnings rule)

   use all type Rule_Params;

   function "=" (L, R : Parametrized_Exemption_Info) return Boolean is
     (L.SF = R.SF                                         and then
      L.Params = R.Params                                 and then
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

   package Rule_Param_Exempt_Sections_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Rule_Id,
        Element_Type    => Parametrized_Exemption_Sections.Set,
        Hash            => Hash,
        Equivalent_Keys => "=",
        "="             => Parametrized_Exemption_Sections."=");
   Rule_Param_Exempt_Sections : Rule_Param_Exempt_Sections_Map.Map;
   --  Map to store parameteric exemption sections mapped from their rule
   --  identifier.

   function Parse_Exempt_Parameters
     (Rule  : Rule_Id;
      Input : String;
      SF    : SF_Id;
      SLOC  : String) return Exemption_Parameters.Set;
   --  Assuming that ``Rule`` is a rule that allows parametric expressions,
   --  and ``Input`` contains rule parameters, parses ``Input`` string and
   --  checks if each of the specified parameters indeed can be used as rule
   --  exemption parameter. Returns a parsed set of parameters.
   --
   --  If parameters are incorrect, diagnoses will be generated with the given
   --  ``SF`` and ``SLOC``.

   function Allows_Parametrized_Exemption (Rule : Rule_Id) return Boolean;
   --  Checks if Rule allows fine-tuned exemption (with specifying parameters
   --  for that it can be exempted). Assumes Present (Rule).

   function Allowed_As_Exemption_Parameter
     (Rule  : Rule_Id;
      Param : String) return Boolean;
   --  Checks if Param is allowed as a rule parameter in rule exemption pragma.
   --  Assumes that Param has already folded to lower case. Always returns
   --  False if Allows_Prametrized_Exemption (Rule) is False.

   function Is_Param_Exempted (Rule : Rule_Id) return Boolean;
   --  Returns whether ``Rule`` is already exempted with any actual parameters.

   function Is_Exempted_With_Pars
     (Rule : Rule_Id; Params : Exemption_Parameters.Set)
   return Parametrized_Exemption_Sections.Cursor;
   --  Checks if ``Rule`` is already exempted with the set of parameters that
   --  are stored in ``Params``. If it is, then return a cursor pointing to the
   --  corresponding exemption section in Rule_Param_Exempt_Sections, otherwise
   --  return ``No_Element``.

   function Find_Same_Parameter_Exemption
     (Rule        : Rule_Id;
      Params      : Exemption_Parameters.Set;
      Par         : out Unbounded_String)
      return Parametrized_Exemption_Sections.Cursor;
   --  Is similar to the previous procedure, but it checks that there is at
   --  least one parameter in ``Params`` that has already been used in
   --  definition of some opened parametric exception section for this
   --  rule::
   --
   --      Exempt_On, Rule:Par1, Par3
   --      ...
   --      Exempt_On, Rule:Par1, Par2
   --
   --  If the checks succeeds, ``Par`` is set to point to this parameter.

   function Get_Param_Justification
     (Rule : Rule_Id;
      Diag : String;
      SF   : SF_Id;
      Line : Natural;
      Col  : Natural) return Unbounded_String;
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
   --  Param is supposed to correspond to (choosing the right set of exemption
   --  sections should be done outside the call) with Param.  Returns
   --  No_Element if the attempt fails.

   procedure Increase_Diag_Counter
     (Exem_Sections : in out Parametrized_Exemption_Sections.Set;
      Section       : Parametrized_Exemption_Sections.Cursor);
   --  Adds 1 to the counter of detected violations for the exemption sections
   --  pointed by Section in Exem_Sections.

   procedure Turn_Off_Parametrized_Exemption
     (Rule         : Rule_Id;
      Exempted_At  : in out Parametrized_Exemption_Sections.Cursor;
      Closing_Sloc : Source_Location;
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

   function New_Postponed_Check_Exemption_Sections_Array
     return Postponed_Check_Exemption_Sections_Array_Access is
       (new Postponed_Check_Exemption_Sections_Array
          (First_SF_Id .. Last_Argument_Source));
   --  Allocate a new Postponed_Check_Exemption_Sections_Array and return the
   --  address.

   package Postponed_Exemption_Sections_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Rule_Id,
        Element_Type    => Postponed_Check_Exemption_Sections_Array_Access,
        Hash            => Hash,
        Equivalent_Keys => "=");
   Postponed_Exemption_Sections : Postponed_Exemption_Sections_Map.Map;
   --  For each argument source, stores all the exemption sections found in
   --  this source. These sections are stored as they are processed - that is,
   --  in alphabetical order. Sections for different kinds of checks are stored
   --  separately.

   procedure Map_On_Postponed_Check_Exemption
     (In_File       : SF_Id;
      For_Check     : Rule_Id;
      For_Line      : Positive;
      Is_Exempted   : out Boolean;
      Justification : in out Unbounded_String);
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

   function New_Per_Source_Postponed_Param_Exemp
     return Per_Source_Postponed_Param_Exemp_Access is
       (new Per_Source_Postponed_Param_Exemp
          (First_SF_Id .. Last_Argument_Source));
   --  Allocate a new Per_Source_Postponed_Param_Exemp and return the addess

   package Per_Rule_Postponed_Param_Exemp_Map is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Rule_Id,
        Element_Type    => Per_Source_Postponed_Param_Exemp_Access,
        Hash            => Hash,
        Equivalent_Keys => "=");
   Postponed_Param_Exempt_Sections : Per_Rule_Postponed_Param_Exemp_Map.Map;

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
     (Rule : Rule_Id; Param : String) return Boolean
   is
   begin
      if not Allows_Parametrized_Exemption (Rule) then
         return False;
      end if;

      if Rule = Restrictions_Id then
         return Is_Restriction_Exemption_Par (Param);
      elsif Rule = Style_Checks_Id then
         return Is_Style_Exemption_Par (Param);
      elsif Rule = Warnings_Id then
         return Is_Warning_Exemption_Par (Param);
      else
         return All_Rules (Rule).Allowed_As_Exemption_Parameter (Param);
      end if;
   end Allowed_As_Exemption_Parameter;

   -----------------------------------
   -- Allows_Parametrized_Exemption --
   -----------------------------------

   function Allows_Parametrized_Exemption (Rule : Rule_Id) return Boolean is
   begin
      if Is_Compiler_Rule (Rule) then
         return True;
      else
         return All_Rules (Rule).Allows_Parametrized_Exemption;
      end if;
   end Allows_Parametrized_Exemption;

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
      Sloc : constant Source_Location := End_Sloc (Unit.Root.Sloc_Range);
      use Parametrized_Exemption_Sections;

      Next_Section : Parametrized_Exemption_Sections.Cursor;
      Rule         : Rule_Id;
      To_Turn_Off  : Rule_Id_Vec.Vector;
   begin
      --  Non-parametric exemptions:

      for Cursor in Exemption_Sections.Iterate loop
         Rule := Exemption_Sections_Map.Key (Cursor);
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
            To_Turn_Off.Append (Rule);
         end if;
      end loop;

      for Cursor in To_Turn_Off.Iterate loop
         Turn_Off_Exemption
           (Rule         => To_Turn_Off (Cursor),
            Closing_Sloc => Sloc,
            SF           => SF);
      end loop;
      To_Turn_Off.Clear;

      --  Parametric exemptions:

      for Cursor in Rule_Param_Exempt_Sections.Iterate loop
         Rule := Rule_Param_Exempt_Sections_Map.Key (Cursor);
         if Allows_Parametrized_Exemption (Rule)
           and then not Is_Empty (Rule_Param_Exempt_Sections (Cursor))
         then
          --  We cannot use set iterator here - we need to use Rule and SF
          --  into processing routine

            Next_Section := First (Rule_Param_Exempt_Sections (Cursor));

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
                 (Rule, Next_Section, Sloc, SF);
               Next_Section := First
                 (Rule_Param_Exempt_Sections (Cursor));
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
            when Rule_Violation =>
               if Error_Messages_Storage.Element (Position).Justification =
                  Null_Unbounded_String
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

   ----------------
   -- Escape_XML --
   ----------------

   function Escape_XML (S : String) return String is
      Result : Unbounded_String;
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

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Diag_Message) return Boolean is
   begin
      return L.File < R.File
         or else (L.File = R.File and then L.Sloc < R.Sloc)
         or else (L.File = R.File
                  and then L.Sloc = R.Sloc
                  and then L.Text < R.Text);
   end "<";

   -----------------------------------
   -- Generate_Qualification_Report --
   -----------------------------------

   procedure Generate_Qualification_Report is
      use all type GNAT.OS_Lib.String_Access;
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

      Result               : Parametrized_Exemption_Sections.Cursor
        := Parametrized_Exemption_Sections.No_Element;
      Next_Section         : Parametrized_Exemption_Sections.Cursor
        := First (Exem_Sections);

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
           and then Next_Section_El.Params.Contains (Param)
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
      Col  : Natural) return Unbounded_String
   is
      use Parametrized_Exemption_Sections;
      Exem_Sections  : Parametrized_Exemption_Sections.Set
        renames Postponed_Param_Exempt_Sections (Rule) (SF);
      Fit_In_Section : Parametrized_Exemption_Sections.Cursor;

      Param : constant String := Rule_Parameter (Diag, Rule);
      pragma Assert (Param /= "" or else Rule = Warnings_Id);
   begin
      Fit_In_Section := Get_Exem_Section (Exem_Sections, Param, Line, Col);

      if Has_Element (Fit_In_Section) then
         Increase_Diag_Counter (Exem_Sections, Fit_In_Section);
         return Parametrized_Exemption_Sections
                .Element (Fit_In_Section).Exempt_Info.Justification;
      end if;

      return Null_Unbounded_String;
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

   -----------------
   -- Is_Exempted --
   -----------------

   function Is_Exempted (Rule : Rule_Id) return Boolean is
   begin
      return Exemption_Sections.Contains (Rule)
        and then Exemption_Sections (Rule).Line_Start > 0;
   end Is_Exempted;

   -----------------------
   -- Is_Param_Exempted --
   -----------------------

   function Is_Param_Exempted (Rule : Rule_Id) return Boolean is
   begin
      return Rule_Param_Exempt_Sections.Contains (Rule)
        and then not Rule_Param_Exempt_Sections (Rule).Is_Empty;
   end Is_Param_Exempted;

   ---------------------------
   -- Is_Exempted_With_Pars --
   ---------------------------

   function Is_Exempted_With_Pars
     (Rule   : Rule_Id;
      Params : Exemption_Parameters.Set)
   return Parametrized_Exemption_Sections.Cursor
   is
      use Parametrized_Exemption_Sections;
      Next_Section : Parametrized_Exemption_Sections.Cursor;
      Ret : Parametrized_Exemption_Sections.Cursor :=
         Parametrized_Exemption_Sections.No_Element;
   begin
      if Is_Param_Exempted (Rule) then
         Next_Section := First (Rule_Param_Exempt_Sections (Rule));

         while Has_Element (Next_Section) loop
            if Params =
              Parametrized_Exemption_Sections.Element (Next_Section).Params
            then
               Ret := Next_Section;
               exit;
            end if;

            Next_Section := Next (Next_Section);
         end loop;
      end if;

      return Ret;
   end Is_Exempted_With_Pars;

   ---------------------------------------
   -- Map_On_Postponed_Check_Exemption --
   ---------------------------------------

   procedure Map_On_Postponed_Check_Exemption
     (In_File       : SF_Id;
      For_Check     : Rule_Id;
      For_Line      : Positive;
      Is_Exempted   : out Boolean;
      Justification : in out Unbounded_String)
   is
      Section : Postponed_Rule_Exemption_Info_Access;
   begin
      Is_Exempted := False;

      if not Is_Argument_Source (In_File)
        or else not Postponed_Exemption_Sections.Contains (For_Check)
      then
         --  Exemption sections are processed in argument files only. Also
         --  rule must be exempted.
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

         if ES_Info.Params.Is_Empty then
            Info ("   Empty parameter list!");
         else
            for El of ES_Info.Params loop
               Info (">>" & El & "<<");
            end loop;
         end if;

         Info ("Span: " & ES_Info.Exempt_Info.Line_Start'Img & ":" &
                ES_Info.Exempt_Info.Col_Start'Img & " -"  &
                ES_Info.Exempt_Info.Line_End'Img & ":" &
                ES_Info.Exempt_Info.Col_End'Img);
         Info ("Detected :" & ES_Info.Exempt_Info.Detected'Img);
         Info_No_EOL ("Justification: ");

         if ES_Info.Exempt_Info.Justification = Null_Unbounded_String then
            Info ("IS NOT SET!!!");
         else
            Info (">>" & To_String (ES_Info.Exempt_Info.Justification) & "<<");
         end if;
      else
         Info ("No parametric exemption section");
      end if;
   end Parametrized_Exem_Section_Debug_Image;

   ----------------
   -- Params_Img --
   ----------------

   function Params_Img (Params : Rule_Params; Rule : Rule_Id) return String
   is
      Res   : Unbounded_String;
      Count : Natural := 0;
   begin
      for El of Params loop
         if Count > 0 then
            Append (Res, ", ");
         end if;

         Append
           (Res,
            (if Rule in Warnings_Id | Style_Checks_Id
             then El
             else GNAT.Case_Util.To_Mixed (El)));

         Count := Count + 1;
      end loop;

      return To_String (Res);
   end Params_Img;

   -----------------------------
   -- Print_Active_Rules_File --
   -----------------------------

   procedure Print_Active_Rules_File is
      Rule_List_File : Ada.Text_IO.File_Type;

      use all type GNAT.OS_Lib.String_Access;
   begin
      if Text_Report_ON then
         Report_No_EOL ("coding standard   : ");
      end if;

      if XML_Report_ON then
         XML_Report_No_EOL
           ("<coding-standard from-file=""", Indent_Level => 1);
      end if;

      if not Individual_Rules_Set
        and then Rule_File_Name /= null
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
            if GNAT.OS_Lib.Is_Regular_File (Full_Rule_List_File_Name) then
               Open (Rule_List_File, Out_File, Full_Rule_List_File_Name);
            else
               Create (Rule_List_File, Out_File, Full_Rule_List_File_Name);
            end if;

            for Cursor in All_Rule_Instances.Iterate loop
               Print_Rule_Instance_To_File
                 (All_Rule_Instances (Cursor).all,
                  Rule_List_File);
               New_Line (Rule_List_File);
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
         for Cursor in All_Rule_Instances.Iterate loop
            XML_Print_Rule_Instance (All_Rule_Instances (Cursor).all, 2);
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
              and then
                Print_Exempted_Violations =
                  (Diag.Justification = Null_Unbounded_String)
            then
               return;
            end if;

            if Text_Report_ON then
               Report (Strip_Tag (Image (Diag)));

               if Diag.Justification /= Null_Unbounded_String then
                  Report ("(" & To_String (Diag.Justification) & ")", 1);
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

         if GNAT.OS_Lib.Is_Regular_File (Full_Source_List_File_Name) then
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

         if GNAT.OS_Lib.Is_Regular_File (Full_Ignored_Source_List_File_Name)
         then
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
                            Strip_Tag (Diag (Idx + 2 .. Diag'Last));
            end;
         else
            return Strip_Tag (Diag);
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
               Diagnoses_Reported >= Max_Diagnoses
            then
               Limit_Exceeded := True;
               Info ("Maximum diagnoses reached, " &
                     "see the report file for full details");
            else
               if Error_Messages_Storage.Element (Position).Justification
                    = Null_Unbounded_String
               then
                  Diagnoses_Reported := @ + 1;
                  Info
                    (Preprocess_Diag
                      (Image
                         (Error_Messages_Storage.Element (Position))));
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

   ---------------------------
   -- Process_Exempt_Action --
   ---------------------------

   procedure Process_Exempt_Action (Self : Exempt_Action) is
      SF              : constant SF_Id := File_Find (Self.Unit.Get_Filename);
      Rule_Name       : constant String := To_String (Self.Rule_Name);
      Rule            : constant Rule_Id := Get_Rule (Rule_Name);
      Has_Params      : constant Boolean := not Self.Params.Is_Empty;
      Exempted_At     : Parametrized_Exemption_Sections.Cursor;
      Sloc_Start      : constant Source_Location :=
        Langkit_Support.Slocs.Start_Sloc (Self.Sloc_Range);
      Sloc_End        : constant Source_Location :=
        Langkit_Support.Slocs.End_Sloc (Self.Sloc_Range);

      use Parametrized_Exemption_Sections;

      Action : Exempt_Action := Self;
   begin
      --  First part: Legality checks
      if Self.Exemption_Control = Not_An_Exemption then
         Store_Diagnosis
           (Full_File_Name     => Self.Unit.Get_Filename,
            Sloc               => Sloc_Start,
            Message            => "wrong exemption kind, ignored",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
         return;
      end if;

      --  3.1 Check if we have a rule corresponding to the provided name
      if not Present (Rule) then
         Store_Diagnosis
           (Full_File_Name     => Self.Unit.Get_Filename,
            Sloc               => Sloc_Start,
            Message            => "wrong rule name in exemption (" &
                                  Rule_Name & "), ignored",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
         return;
      end if;

      --  Verify that, if the rule has params, it's allowed to have some
      if Self.Params.Length > 0
         and then not Allows_Parametrized_Exemption (Rule)
      then
         Store_Diagnosis
           (Full_File_Name     => Self.Unit.Get_Filename,
            Sloc               => Sloc_Start,
            Message            => "rule " & Rule_Name &
                                  " cannot have parametric " &
                                  "exemption, ignored",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
         return;
      end if;

      --  Justification is not expected (and shouldn't be present) if the
      --  action is to turn off an exemption.
      if Action.Exemption_Control = Exempt_Off
         and then Action.Justification /= Null_Unbounded_String
         and then Action.Check_Justification
      then
         Store_Diagnosis
           (Full_File_Name     => Action.Unit.Get_Filename,
            Sloc               => Sloc_Start,
            Message            =>
              "turning exemption OFF does not need justification",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
      end if;

      --  6. If exemption is turned ON, justification is expected
      if Action.Exemption_Control = Exempt_On
         and then Action.Justification = Null_Unbounded_String
         and then Action.Check_Justification
      then
         Store_Diagnosis
           (Full_File_Name     => Action.Unit.Get_Filename,
            Sloc               => Sloc_Start,
            Message            =>
              "turning exemption ON expects justification",
            Diagnosis_Kind     => Exemption_Warning,
            SF                 => SF);
      end if;

      --  If `Rule` is not enabled - nothing to do
      if not
        (Is_Enabled (Rule) or else
         (Rule = Warnings_Id and then Is_Enabled (Restrictions_Id)))
      then
         --  In case when a Restriction rule is enabled, we may want to use
         --  exemptions section for Warnings rule to suppress default warnings.
         --  We may get rid of this if and when we get a possibility to turn
         --  off all the warnings except related to restrictions only.
         return;

      --  If the exempted rule is "Restrictions" and there are parameters, we
      --  want to ensure that at least one of the specified restrictions is
      --  enabled.
      elsif Rule = Restrictions_Id and then Has_Params then
         declare
            Active_Found : Boolean := False;
         begin
            for Param of Action.Params loop
               declare
                  Sep_Idx : constant Natural := Index (Param, "=");
                  Restriction_Name : constant String :=
                    (if Sep_Idx = 0
                     then Param
                     else Param (Param'First .. Sep_Idx - 1));
               begin
                  if Is_Restriction_Active (Restriction_Name) then
                     Active_Found := True;
                  end if;
               end;
            end loop;
            if not Active_Found then
               return;
            end if;
         end;
      end if;

      --  Now - processing of the exemption pragma. If we are here, we are
      --  sure that Rule denotes an existing rule and Instance is an instance
      --  of it.
      case Action.Exemption_Control is
         when Exempt_On =>

            if Action.Justification = Null_Unbounded_String then
               Action.Justification := To_Unbounded_String ("unjustified");
            end if;

            if Is_Exempted (Rule) then
               Store_Diagnosis
                 (Full_File_Name     => Action.Unit.Get_Filename,
                  Sloc               => Sloc_Start,
                  Message            =>
                    "rule " & Rule_Name &
                    " is already exempted at line" &
                    Exemption_Sections (Rule).Line_Start'Img,
                  Diagnosis_Kind     => Exemption_Warning,
                  SF                 => SF);
               return;
            end if;

            if not Has_Params and then Allows_Parametrized_Exemption (Rule)
            then
               --  Is Rule already exempted with parameters?
               if Is_Param_Exempted (Rule) then
                  Store_Diagnosis
                    (Full_File_Name     => Action.Unit.Get_Filename,
                     Sloc               => Sloc_Start,
                     Message            =>
                       "rule " & Rule_Name
                       & " is already exempted with parameter(s) at line"
                       & Element (First (Rule_Param_Exempt_Sections (Rule)))
                         .Exempt_Info.Line_Start'Img,
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  return;
               end if;
            end if;

            if Has_Params then
               Exempted_At := Is_Exempted_With_Pars (Rule, Action.Params);

               if Has_Element (Exempted_At)
               then
                  Store_Diagnosis
                    (Full_File_Name     => Action.Unit.Get_Filename,
                     Sloc               => Sloc_Start,
                     Message            =>
                       "rule " & Rule_Name &
                       " is already exempted with the same parameters at line"
                       & Element (Exempted_At).Exempt_Info.Line_Start'Img,
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  return;
               else
                  --  We have to check the following:
                  --
                  --     Exempt_On, Rule:Par1, Par3
                  --     ...
                  --     Exempt_On, Rule:Par1, Par2

                  declare
                     Param : Unbounded_String;
                  begin
                     Exempted_At := Find_Same_Parameter_Exemption
                       (Rule, Action.Params, Param);

                     if Has_Element (Exempted_At) then
                        Store_Diagnosis
                          (Full_File_Name     => Action.Unit.Get_Filename,
                           Sloc               => Sloc_Start,
                           Message            =>
                             "rule " & Rule_Name
                             & " is already exempted with parameter '"
                             & To_String (Param) & "' at line"
                             & Element (Exempted_At)
                               .Exempt_Info.Line_Start'Img,
                           Diagnosis_Kind     => Exemption_Warning,
                           SF                 => SF);

                        return;
                     end if;
                  end;

                --  If we are here then we know for sure that the
                --  parametric exemption is correct, and there is no
                --  open exemption section for this rule and this
                --  parameter(s). So we can just add the corresponding record
                --  to Rule_Param_Exempt_Sections:

                  if not Rule_Param_Exempt_Sections.Contains (Rule) then
                     Rule_Param_Exempt_Sections.Insert
                       (Rule, Parametrized_Exemption_Sections.Empty);
                  end if;
                  Insert
                    (Rule_Param_Exempt_Sections (Rule),
                     (Exempt_Info =>
                        (Line_Start    => Natural (Sloc_Start.Line),
                         Col_Start     => Natural (Sloc_Start.Column),
                         Line_End      => 0,
                         Col_End       => 0,
                         Justification => Action.Justification,
                         Detected      => 0),
                      Rule        => Rule,
                      SF          => SF,
                      Params      => Action.Params));
               end if;
            else
               Exemption_Sections.Insert
                 (Rule,
                  (Line_Start    => Natural (Sloc_Start.Line),
                   Col_Start     => Natural (Sloc_Start.Column),
                   Line_End      => 0,
                   Col_End       => 0,
                   Justification => Action.Justification,
                   Detected      => 0));
            end if;

         when Exempt_Off =>
            if Has_Params then
               Exempted_At := Is_Exempted_With_Pars (Rule, Action.Params);

               if not Parametrized_Exemption_Sections.Has_Element (Exempted_At)
               then
                  Store_Diagnosis
                    (Full_File_Name     => Action.Unit.Get_Filename,
                     Sloc               => Sloc_Start,
                     Message            =>
                       "rule " & Rule_Name & " is not in exempted state",
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  return;
               else
                  Turn_Off_Parametrized_Exemption
                    (Rule, Exempted_At, Sloc_End, SF);
               end if;
            else
               if not Is_Exempted (Rule) then
                  Store_Diagnosis
                    (Full_File_Name     => Action.Unit.Get_Filename,
                     Sloc               => Sloc_Start,
                     Message            =>
                       "rule " & Rule_Name & " is not in exempted state",
                     Diagnosis_Kind     => Exemption_Warning,
                     SF                 => SF);
                  return;
               end if;

               Turn_Off_Exemption (Rule, Sloc_End, SF);
            end if;

         when Not_An_Exemption =>
            pragma Assert (False);
      end case;
   end Process_Exempt_Action;

   -------------------------------
   -- Process_Exemption_Comment --
   -------------------------------

   procedure Process_Exemption_Comment
     (El : LAL.Common.Token_Reference; Unit : LAL.Analysis.Analysis_Unit)
   is
      Text    : constant String := Image (LAL.Common.Text (El));
      SF      : constant SF_Id := File_Find (Unit.Get_Filename);
      Matches : Match_Array (0 .. 4);
   begin
      --  Early out to not try and match comments that don't have the expected
      --  syntax.
      if Text'Last < 4 or else Text (1 .. 4) /= "--##" then
         return;
      end if;

      Match (Match_Exempt_Comment, Text, Matches);

      if Matches (0) = No_Match then
         --  We don't issue a warning here, because, it's possible (however
         --  unlikely) that some people are using the "--##" syntax for
         --  other things.
         return;
      end if;

      declare
         Is_Line : constant Boolean := Matches (1) /= No_Match;
         State : String
           renames Text (Matches (2).First .. Matches (2).Last);
         Rule : constant String :=
           To_XString (Text (Matches (3).First .. Matches (3).Last))
           .Trim.To_String;

         Just  : constant String :=
           (if Matches (4) = No_Match
            then ""
            else To_XString (Text (Matches (4).First .. Matches (4).Last))
                 .Trim.To_String);

         use LAL.Common;
      begin
         if Is_Line then
            if State = "on" then
               Store_Diagnosis
                 (Full_File_Name => Unit.Get_Filename,
                  Sloc           => Langkit_Support.Slocs.Start_Sloc
                    (Sloc_Range (Data (El))),
                  Message        =>
                    "State should be ""off"" for line exemption",
                  Diagnosis_Kind => Exemption_Warning,
                  SF             => SF);
            end if;

            declare
               Sloc : constant Source_Location_Range :=
                 (Sloc_Range (Data (El)) with delta Start_Column => 1);
            begin
               --  In order, emit one action to turn the exempt on, and the
               --  other to turn the exempt off, on the same line.
               --  ``Process_Exempt_Action`` will take the start sloc for the
               --  exempt on, and the end sloc for the exempt off.
               for Exempt_Kind in Exempt_On .. Exempt_Off loop
                  Process_Exempt_Action
                    ((Exempt_Kind,
                      To_Unbounded_String (Rule),
                      Params              => <>,
                      Justification       => To_Unbounded_String (Just),
                      Check_Justification => False,
                      Sloc_Range          => Sloc,
                      Unit                => Unit));
               end loop;
            end;
         else
            Process_Exempt_Action
              (((if State = "on" then Exempt_Off
                 elsif State = "off" then Exempt_On
                 else raise Constraint_Error with "should not happen"),
                To_Unbounded_String (Rule),
                Params => <>,
                Justification => To_Unbounded_String (Just),

                --  With this syntax, we don't want to enforce the
                --  justification rules that we have for pragmas.
                Check_Justification => False,
                Sloc_Range => Sloc_Range (Data (El)),
                Unit => Unit));
         end if;
      end;

   end Process_Exemption_Comment;

   ------------------------------
   -- Process_Exemption_Pragma --
   ------------------------------

   procedure Process_Exemption_Pragma (El : LAL.Analysis.Pragma_Node) is
      Pragma_Args : constant LAL.Analysis.Base_Assoc_List := El.F_Args;
      SF          : constant SF_Id := File_Find (El.Unit.Get_Filename);
      Action      : Exempt_Action;

      use Gnatcheck.Rules.Exemption_Parameters;
      use LCO;
   begin
      Action.Unit := El.Unit;
      Action.Sloc_Range := El.Sloc_Range;

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
      Action.Exemption_Control := Get_Exemption_Kind
        (Pragma_Args.List_Child (2).P_Assoc_Expr.Text);

      --  3. Third parameter should be the name of some existing rule, may be,
      --     with parameter names, but the latter is allowed only if fine-tuned
      --     exemptions is allowed for the rule, and if parameter names make
      --     sense for the given rule:
      declare
         Matches : Match_Array (0 .. 2);
         Text    : constant String :=
           Image (Pragma_Args.List_Child (3).P_Assoc_Expr.Text);
      begin
         Match (Match_Rule_Name, Text, Matches);

         pragma Assert (Matches (0) /= No_Match, "failed parsing rule name");

         Action.Rule_Name := To_Unbounded_String
           (Text (Matches (1).First .. Matches (1).Last));

         if Matches (2) /= No_Match then
            --  NOTE: This is sub-optimal, we still need to parse the rule
            --  kind here, because parameters parsing depends on the rule.
            Action.Params := Parse_Exempt_Parameters
              (Get_Rule (To_String (Action.Rule_Name)),
               Text (Matches (2).First .. Matches (2).Last),
               SF,
               Sloc_Image (Start_Sloc (El.Sloc_Range)));

            if Is_Empty (Action.Params) then
               Store_Diagnosis
                 (Full_File_Name     => El.Unit.Get_Filename,
                  Sloc               => Start_Sloc (El.Sloc_Range),
                  Message            => "Invalid parameters",
                  Diagnosis_Kind     => Exemption_Warning,
                  SF                 => SF);
            end if;
         end if;
      end;

      --  4. Fourth parameter, if present, should be a string.
      if Pragma_Args.Children_Count >= 4 then
         --  Evaluate the static string expression of the fourth parameter via
         --  Libadalang.Expr_Eval.
         begin
            declare
               use Libadalang.Expr_Eval;
               Eval_Res : constant Eval_Result :=
                 Expr_Eval (Pragma_Args.List_Child (4).P_Assoc_Expr);
            begin
               Action.Justification :=
                 To_Unbounded_String (Image (To_Text (As_String (Eval_Res))));
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

               --  We already notified the user of the problem, make sure we
               --  won't emit another warning due to the absence of
               --  justification.
               Action.Check_Justification := False;
         end;
      end if;

      if Pragma_Args.Children_Count > 4 then
         Store_Diagnosis
           (Full_File_Name => El.Unit.Get_Filename,
            Sloc           => Start_Sloc (El.Sloc_Range),
            Message        =>
              "rule exemption may have at most 4 parameters",
            Diagnosis_Kind => Exemption_Warning,
            SF             => SF);
      end if;

      --  Process the resulting exempt action
      Process_Exempt_Action (Action);

   end Process_Exemption_Pragma;

   ----------------------------------
   -- Process_Postponed_Exemptions --
   ----------------------------------

   procedure Process_Postponed_Exemptions is
      Next_Postponed_Section  : Postponed_Rule_Exemption_Info_Access;
      Current_Rule            : Rule_Id;
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

         if Diag.Justification /= Null_Unbounded_String then
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

         Diag_Line := Positive (Diag.Sloc.Line);

         Map_On_Postponed_Check_Exemption
           (In_File       => SF,
            For_Check     => Diag.Rule,
            For_Line      => Diag_Line,
            Is_Exempted   => Is_Exempted,
            Justification => Diag.Justification);

         --  Check for parametric exemptions

         if not Is_Exempted
           and then Allows_Parametrized_Exemption (Diag.Rule)
           and then Postponed_Param_Exempt_Sections.Contains (Diag.Rule)
           and then not Parametrized_Exemption_Sections.Is_Empty
                          (Postponed_Param_Exempt_Sections (Diag.Rule) (SF))
         then
            Diag_Column := Positive (Diag.Sloc.Column);
            Diag.Justification :=
              Get_Param_Justification
                (Rule => Diag.Rule,
                 Diag => To_String (Diag.Text),
                 SF   => SF,
                 Line => Diag_Line,
                 Col  => Diag_Column);
         end if;

         if Diag.Justification /= Null_Unbounded_String then
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

      for SF in First_SF_Id .. Last_Argument_Source loop
         --  Non-parametric exemption
         for Cursor in Postponed_Exemption_Sections.Iterate loop
            Current_Rule := Postponed_Exemption_Sections_Map.Key (Cursor);
            Next_Postponed_Section :=
              Postponed_Exemption_Sections (Current_Rule) (SF);

            while Next_Postponed_Section /= null loop
               if Next_Postponed_Section.Exemption_Section.Detected = 0 then
                  Store_Diagnosis
                    (Text           =>
                       File_Name (SF) & ':' &
                       Sloc_Image
                         (Next_Postponed_Section.Exemption_Section.Line_End,
                          Next_Postponed_Section.Exemption_Section.Col_End) &
                       ": no detection for " & Rule_Name (Current_Rule) &
                       " rule in exemption section starting at line" &
                       Next_Postponed_Section.Exemption_Section.Line_Start'Img,
                     Diagnosis_Kind => Exemption_Warning,
                     SF             => SF);
               end if;

               Next_Postponed_Section :=
                 Next_Postponed_Section.Next_Exemption_Section;
            end loop;
         end loop;

         --  Parametric exemptions
         for Cursor in Postponed_Param_Exempt_Sections.Iterate loop
            Current_Rule := Per_Rule_Postponed_Param_Exemp_Map.Key (Cursor);
            if Allows_Parametrized_Exemption (Current_Rule)
              and then not Is_Empty
                (Postponed_Param_Exempt_Sections (Current_Rule) (SF))
            then
               Next_Post_Param_Section := First
                 (Postponed_Param_Exempt_Sections (Current_Rule) (SF));

               while Has_Element (Next_Post_Param_Section) loop
                  Next_Par_S_Info := Parametrized_Exemption_Sections.Element
                    (Next_Post_Param_Section);

                  if Next_Par_S_Info.Exempt_Info.Detected = 0 then
                     Store_Diagnosis
                       (Text           =>
                          File_Name (SF) & ':' &
                          Sloc_Image
                            (Next_Par_S_Info.Exempt_Info.Line_End,
                             Next_Par_S_Info.Exempt_Info.Col_End) &
                          ": no detection for '" &
                          Rule_Name (Current_Rule) & ": " &
                          Params_Img (Next_Par_S_Info.Params, Current_Rule) &
                          "' rule in exemption section starting " & "at line" &
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
      use all type GNAT.OS_Lib.String_Access;
   begin
      if GNAT.OS_Lib.Is_Regular_File (Fname) then
         if User_Info_File /= null then
            Error ("--include-file option can be given only once, " &
                   "all but first ignored");
         else
            User_Info_File           := new String'(Fname);
            User_Info_File_Full_Path := new String'
              (GNAT.OS_Lib.Normalize_Pathname
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
      if Rule = Restrictions_Id then
         return Restriction_Rule_Parameter (Diag);
      elsif Rule = Warnings_Id then
         return Warning_Rule_Parameter (Diag);
      elsif Rule = Style_Checks_Id then
         return Style_Rule_Parameter (Diag);
      else
         return All_Rules (Rule).Rule_Param_From_Diag (Diag);
      end if;
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

   -----------------------------------
   -- Find_Same_Parameter_Exemption --
   -----------------------------------

   function Find_Same_Parameter_Exemption
     (Rule        : Rule_Id;
      Params      : Exemption_Parameters.Set;
      Par         : out Unbounded_String)
      return Parametrized_Exemption_Sections.Cursor
   is
      Next_Par : Exemption_Parameters.Cursor :=
        Exemption_Parameters.First (Params);

      use Parametrized_Exemption_Sections;
      Next_Section : Parametrized_Exemption_Sections.Cursor;

   begin
      if Is_Param_Exempted (Rule) then
         while Exemption_Parameters.Has_Element (Next_Par) loop
            Next_Section := First (Rule_Param_Exempt_Sections (Rule));

            while Has_Element (Next_Section) loop
               if Element (Next_Section).Params.Contains
                 (Exemption_Parameters.Element (Next_Par))
               then
                  Par := To_Unbounded_String
                    (Exemption_Parameters.Element (Next_Par));
                  return Next_Section;
               end if;

               Next_Section := Next (Next_Section);
            end loop;

            Next_Par := Exemption_Parameters.Next (Next_Par);
         end loop;
      end if;

      return Parametrized_Exemption_Sections.No_Element;
   end Find_Same_Parameter_Exemption;

   -----------------------------
   -- Parse_Exempt_Parameters --
   -----------------------------

   function Parse_Exempt_Parameters
     (Rule  : Rule_Id;
      Input : String;
      SF    : SF_Id;
      SLOC  : String) return Exemption_Parameters.Set
   is
      use Gnatcheck.Rules.Exemption_Parameters;

      Is_Warning  : constant Boolean := Rule in Warnings_Id | Style_Checks_Id;
      --  In case of Warnings rule, we consider parameters one by one. That is,
      --  for "ad.c.d.fg" as Input string we separately store 'a', 'd',
      --  '.c', '.d', '.f' and 'g'

      Params        : Exemption_Parameters.Set;
      Current       : Natural := Input'First;
      Matches       : Match_Array (0 .. 1);
      Param_Matcher : constant Pattern_Matcher :=
        (if Is_Warning then Match_Rule_Warning_Param else Match_Rule_Param);
   begin
      loop
         Match (Param_Matcher, Input, Matches, Current);
         exit when Matches (0) = No_Match;

         declare
            Success  : Boolean;
            Position : Exemption_Parameters.Cursor;
            Stripped : constant String :=
              Remove_Spaces (Input (Matches (1).First .. Matches (1).Last));
            Param : constant String :=
              (if Is_Warning then Stripped else To_Lower (Stripped));
         begin
            if Allowed_As_Exemption_Parameter (Rule, Param) then
               Params.Insert (Param, Position, Success);

               if not Success then
                  Store_Diagnosis
                    (Text           => File_Name (SF) & ":" & SLOC &
                                       ": parameter " & Param &
                                       " duplicated in rule exemption",
                     Diagnosis_Kind => Exemption_Warning,
                     SF             => SF);
               end if;
            else
               Store_Diagnosis
                 (Text           => File_Name (SF) & ":" & SLOC &
                                    ": parameter " & Param &
                                    " is not allowed in exemption for rule " &
                                    Rule_Name (Rule),
                  Diagnosis_Kind => Exemption_Warning,
                  SF             => SF);
            end if;
         end;

         Current := Matches (0).Last + 1;
         exit when Current > Input'Last;
      end loop;

      return Params;
   end Parse_Exempt_Parameters;

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
     (Text           : String;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id := No_Rule_Id;
      Justification  : Unbounded_String := Null_Unbounded_String)
   is
      Matches : Match_Array (0 .. 5);
      Sloc    : Source_Location;
   begin

      Match (Match_Diagnosis, Text, Matches);

      pragma Assert
        (Matches (0) /= No_Match, "Invalid text for diagnostic: " & Text);

      Sloc.Line :=
        Line_Number'Value (Text (Matches (3).First .. Matches (3).Last));
      Sloc.Column :=
        Column_Number'Value (Text (Matches (4).First .. Matches (4).Last));

      Store_Diagnosis
        (Full_File_Name => Text (Matches (1).First .. Matches (1).Last),
         Sloc           => Sloc,
         Message        => Text (Matches (5).First .. Matches (5).Last),
         Diagnosis_Kind => Diagnosis_Kind,
         SF             => SF,
         Rule           => Rule,
         Justification  => Justification);
   end Store_Diagnosis;

   procedure Store_Diagnosis
     (Full_File_Name : String;
      Message        : String;
      Sloc           : Source_Location;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id := No_Rule_Id;
      Justification  : Unbounded_String := Null_Unbounded_String)
   is
      use Ada.Directories;

      File_Name : constant Unbounded_String :=
         To_Unbounded_String
           (if Full_Source_Locations
            then Full_File_Name
            else Simple_Name (Full_File_Name));

      Tmp : Diag_Message :=
        (Text           => To_Unbounded_String (Message),
         Sloc           => Sloc,
         File           => File_Name,
         Justification  => Justification,
         Diagnosis_Kind => Diagnosis_Kind,
         Rule           => Rule,
         SF             => SF);

   begin
      --  We need this check to avoid diagnoses duplication. Our set container
      --  has broken "<" relation, so Insert may add diagnoses that are already
      --  stored in the container (see the documentation for "<" for more
      --  details.

      if not All_Error_Messages.Contains (Tmp) then
         if Diagnosis_Kind = Compiler_Error then
            Set_Source_Status (SF, Not_A_Legal_Source);
         elsif Diagnosis_Kind = Internal_Error then
            Set_Source_Status (SF, Error_Detected);
         end if;

         if Justification /= Null_Unbounded_String
           and then Exemption_Sections.Contains (Rule)
         then
            --  Here we count detections for non-parametric exemption
            --  sections only

            Exemption_Sections (Rule).Detected := @ + 1;
         end if;

         All_Error_Messages.Insert (Tmp, Unused_Position, Unused_Inserted);
      end if;
   end Store_Diagnosis;

   ---------------
   -- Strip_Tag --
   ---------------

   function Strip_Tag (Diag : String) return String is
      Idx : constant Natural := Index (Diag, " [-gnat");
   begin
      return (if Idx = 0 then Diag else Diag (Diag'First .. Idx - 1));
   end Strip_Tag;

   ------------------------
   -- Turn_Off_Exemption --
   ------------------------

   procedure Turn_Off_Exemption
     (Rule : Rule_Id; Closing_Sloc : Source_Location; SF : SF_Id)
   is
      Tmp : Postponed_Rule_Exemption_Info_Access;
   begin
      --  Set the exemption closing source location
      Exemption_Sections (Rule).Line_End := Natural (Closing_Sloc.Line);
      Exemption_Sections (Rule).Col_End  := Natural (Closing_Sloc.Column);

      --  If the map of postponed exemptions doesn't contains exemptions
      --  related to `Rule`, then create a new array associated to this rule.
      if not Postponed_Exemption_Sections.Contains (Rule) then
         Postponed_Exemption_Sections.Insert
           (Rule, New_Postponed_Check_Exemption_Sections_Array);
      end if;

      --  Add the exemption to the postponed exemption list
      Tmp := new Postponed_Rule_Exemption_Info;
      Tmp.Exemption_Section := Exemption_Sections (Rule);
      Tmp.Next_Exemption_Section := Postponed_Exemption_Sections (Rule) (SF);
      Postponed_Exemption_Sections (Rule) (SF) := Tmp;

      --  Remove the exemption from the map
      Exemption_Sections.Delete (Rule);
   end Turn_Off_Exemption;

   -------------------------------------
   -- Turn_Off_Parametrized_Exemption --
   -------------------------------------

   procedure Turn_Off_Parametrized_Exemption
     (Rule         : Rule_Id;
      Exempted_At  : in out Parametrized_Exemption_Sections.Cursor;
      Closing_Sloc : Source_Location;
      SF           : SF_Id)
   is
      New_Section : Parametrized_Exemption_Info;
   begin
      --  Set the exemption section closing source location
      New_Section := Parametrized_Exemption_Sections.Element (Exempted_At);
      New_Section.Exempt_Info.Line_End := Natural (Closing_Sloc.Line);
      New_Section.Exempt_Info.Col_End  := Natural (Closing_Sloc.Column);

      --  Create an posponed param exemption array for the rule if it does
      --  not exist.
      if not Postponed_Param_Exempt_Sections.Contains (Rule) then
         Postponed_Param_Exempt_Sections.Insert
           (Rule, New_Per_Source_Postponed_Param_Exemp);
      end if;

      --  Insert the exemption in the postponed list for later handling, then
      --  remove the original exemption from the map.
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
        Diag.Justification /= Null_Unbounded_String;
      Message     : constant String  := Strip_Tag (Image (Diag));
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
           ("<justification>" & Escape_XML (To_String (Diag.Justification)) &
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

   -----------
   -- Image --
   -----------

   function Image (Self : Diag_Message) return String is
      function Image (Sloc : Source_Location) return String;
      --  Custom image function for Langkit source locations, that will add a
      --  leading 0 for columns under 10.

      -----------
      -- Image --
      -----------

      function Image (Sloc : Source_Location) return String is
         Column_Str : constant String :=
           (if Sloc.Column >= 10 then "" else "0")
           & Ada.Strings.Fixed.Trim (Column_Number'Image (Sloc.Column), Left);
      begin
         return
           (Ada.Strings.Fixed.Trim (Line_Number'Image (Sloc.Line), Left)
            & ':' & Column_Str);
      end Image;
   begin
      return To_String (Self.File) & ":" & Image (Self.Sloc) & ": "
        & To_String (Self.Text);
   end Image;

end Gnatcheck.Diagnoses;
