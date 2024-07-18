--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines routines for storing diagnostic messages and
--  generating final gnatcheck report. It also provides routines that
--  supports rule exemption mechanism. Note, that most of the rule exemption
--  mechanism is hidden in the body of the package.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Regpat; use GNAT.Regpat;

with Gnatcheck.Ids;          use Gnatcheck.Ids;
with Gnatcheck.Source_Table; use Gnatcheck.Source_Table;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Libadalang.Analysis;
with Libadalang.Common;

package Gnatcheck.Diagnoses is

   package LAL renames Libadalang;

   ------------------------
   -- Diagnoses matching --
   ------------------------

   Match_Diagnosis : constant Pattern_Matcher :=
     Compile ("^(([A-Z]:)?[^:]*):(\d+):(\d+): (.*)$");
   --  Matcher for a diagnostic

   Match_Rule_Name : constant Pattern_Matcher :=
     Compile ("^""([^\s:]+)\s*(?::\s*(.*))?""$");
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

   type Diagnosis_Kinds is
     (Rule_Violation,
      --  Corresponds to all rule diagnoses, including compiler checks
      Exemption_Warning,
      --  Warnings generated for Annotate pragmas used to implement rule
      --  exemption mechanism.
      Compiler_Error,
      --  Compiler diagnoses generated for illegal (non-compilable) sources
      Internal_Error
      --  Internal gnatcheck error
      );

   procedure Store_Diagnosis
     (Text           : String;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id := No_Rule_Id);
   --  Stores a diagnosis expressed in ``Text`` with the other precisions.
   --  This function use the other ``Store_Diagnosis`` to save the generated
   --  diagnosis in the internal data structure.

   procedure Store_Diagnosis
     (Full_File_Name : String;
      Message        : String;
      Sloc           : Source_Location;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id := No_Rule_Id);
   --  Stores the diagnosis in the internal data structure. The same procedure
   --  is used for all diagnosis kinds, in case of Exemption_Warning,
   --  Compiler_Error and Internal_Error, Rule should be set to No_Rule_Id.

   function Sloc_Image (Line, Column : Natural) return String;
   function Sloc_Image (Sloc : Source_Location) return String;
   --  Return an image of line:column with Column having a leading '0' if less
   --  than 10.

   ------------------------
   -- Diagnoses Counters --
   ------------------------

   Detected_Non_Exempted_Violations : Natural := 0;
   Detected_Exempted_Violations     : Natural := 0;
   --  Separate counters for exempted and non-exempted violations.

   Detected_Exemption_Warning : Natural := 0;
   Detected_Compiler_Error    : Natural := 0;
   Detected_Internal_Error    : Natural := 0;

   -----------------------
   -- Report generation --
   -----------------------

   procedure Generate_Qualification_Report;
   --  Generate the report oriented for using as qualification materials. There
   --  is no parameter to configure this report except
   --  Gnatcheck.Options.Short_Report flag.

   procedure Print_Report_Header;
   --  Generates the report header, including the date, tool version and
   --  tool command liner invocation sequence. (We need it in spec because it
   --  is used by Gnatcheck.Projects.Aggregate_Project_Report_Header.

   procedure Process_User_Filename (Fname : String);
   --  Checks if Fname is the name of the existing file. If it is, sets it as
   --  the value of Gnatcheck.Options.User_Info_File, otherwise generates
   --  warning and leaves User_Info_File unchanged. If User_Info_File is
   --  already set, and Fname denotes some existing file, generates a warning
   --  (user-defined part of the report file can be specified only once!) and
   --  leaves User_Info_File unchanged.

   -------------------------
   -- Exemption mechanism --
   -------------------------

   function Is_Exemption_Pragma (El : LAL.Analysis.Pragma_Node) return Boolean;
   --  Checks if the argument Element is the Annotate or GNAT_Annotate pragma
   --  with the  first parameter equal to 'gnatcheck'.

   procedure Process_Exemption_Pragma (El : LAL.Analysis.Pragma_Node);
   --  Analyses the argument element and stores the
   --  information about exemption section. In most of the cases it is
   --  equivalent to turning the rule into exempted state, but for the
   --  following rule categories:
   --    * compiler checks
   --
   --  post-processing is needed. This postprocessing can be done when all the
   --  rule checking and processing of exemption pragmas on all the sources is
   --  completed.

   procedure Process_Exemption_Comment
     (El : LAL.Common.Token_Reference; Unit : LAL.Analysis.Analysis_Unit);
   --  Process any comment from a source being analyzed. If it is an exemption
   --  comment, process it.
   --
   --  The logic is the same as ``Process_Exemption_Pragma``, only the syntax
   --  differs.

   procedure Check_Unclosed_Rule_Exemptions
     (SF   : SF_Id;
      Unit : LAL.Analysis.Analysis_Unit);
   --  Is supposed to be called in the very end of processing of the source
   --  corresponding to SF. Checks if there exist some exempted rules. For each
   --  such rule, a warning is issued and exemption is turned OFF. Unit
   --  parameter is used to compute the end of non-closed exemption sections
   --  for compiler checks, if any.

end Gnatcheck.Diagnoses;
