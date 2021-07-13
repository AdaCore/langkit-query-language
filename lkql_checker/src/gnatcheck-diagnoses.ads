------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                  G N A T C H E C K . D I A G N O S E S                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2006-2021, AdaCore                     --
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

--  This package defines routines for storing diagnostic messages and
--  generating final gnatcheck report. It also provides routines that
--  supports rule exemption mechanism. Note, that most of the rule exemption
--  mechanism is hidden in the body of the package.

with GNAT.OS_Lib;            use GNAT.OS_Lib;

with Gnatcheck.Source_Table; use Gnatcheck.Source_Table;
with Gnatcheck.Ids;          use Gnatcheck.Ids;

package Gnatcheck.Diagnoses is

   -----------------------
   -- Diagnoses storage --
   -----------------------

   type Diagnosis_Kinds is
     (Not_A_Diagnosis,
      Rule_Violation,
      --  Corresponds to all rule diagnoses, including compiler checks
      Exemption_Warning,
      --  Warnings generated for Annotate pragmas used to implement rule
      --  exemption mechanism
      Compiler_Error,
      --  Compiler diagnoses generated for illegal (non-compilable) sources
      Gnatcheck_Warning
      --  Gnatcheck warnings of any kind
      );

   procedure Store_Diagnosis
     (Text           : String;
      Diagnosis_Kind : Diagnosis_Kinds;
      SF             : SF_Id;
      Rule           : Rule_Id       := No_Rule;
      Justification  : String_Access := null);
   --  Stores the diagnosis in the internal data structure. The same procedure
   --  is used for all diagnosis kinds, in case of Exemption_Warning,
   --  Compiler_Error and Gnatcheck_Warnings Rule should be set to No_Rule.

   ------------------------
   -- Diagnoses Counters --
   ------------------------

   Detected_Non_Exempted_Violations : Natural := 0;
   Detected_Exempted_Violations     : Natural := 0;
   --  Separate counters for exempted and non-exempted violations.

   Detected_Exemption_Warning  : Natural := 0;
   Detected_Compiler_Error     : Natural := 0;
   Detected_Gnatcheck_Warnings : Natural := 0;

   ------------------------
   --  Report generation --
   ------------------------

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

   procedure Init_Postponed_Check_Exemptions;
   --  Prepares the internal data structures for storing information about
   --  postponed checks exemption sections in a source. Should be called in the
   --  very beginning of analyzing a new source. For compiler checks, global
   --  rules, rules that are checked on expanded generics we can do rule
   --  exemptions only after completing processing of all the sources.

   procedure Init_Exemptions;
   --  Initializes all the internal data structures needed for exemption
   --  mechanism

   type Exemption_Pragma_Kinds is
     (Not_An_Exemption_Pragma,
      GNAT_Specific,           --  that is, a normal GNAT Annotate pragma
      Unknown);                --  Unknown pragma, can be used with old
                               --  compilers that do not know the modern
                               --  syntax of GNAT Annotate pragma

--   ###
--   function Exemption_Pragma_Kind (El : Asis.Element)
--     return Exemption_Pragma_Kinds;
     --  Checks if the argument Element is either the GNAT Annotate pragma with
     --  first parameter equal to 'gnatcheck', or some unknown pragma that can
     --  be processed by old compilers and that can be used as an exemption
     --  pragma for gnatcheck

--   ###
--   procedure Process_Exemption_Pragma
--     (El          : Asis.Element;
--      Pragma_Kind : Exemption_Pragma_Kinds);
   --  Should never be called with Pragma_Kind equals to
   --  Not_An_Exemption_Pragma. Analyses the argument element and stores the
   --  information about exemption section. In most of the cases (for local
   --  rules, that are not checked on expanded instantiations) it is
   --  equivalent to turning the rule into exempted state, but for the
   --  following rule categories:
   --    * compiler checks
   --    * global rules
   --    * rules checked on expended instantiations
   --
   --  post-processing is needed. This postprocessing can be done when all the
   --  rule checking and processing of exemption pragmas on all the sources is
   --  completed.

--   ###
--   procedure Check_Unclosed_Rule_Exemptions
--     (SF   : SF_Id;
--      Unit : Asis.Element);
   --  Is supposed to be called in the very end of processing of the source
   --  corresponding to SF. Checks if there exist some exempted rules. For each
   --  such rule, a warning is issued and exemption is turned OFF. Unit
   --  parameter is used to compute the end of non-closed exemption sections
   --  for compiler checks, if any.

   function Exemption_Justification (Rule : Rule_Id) return String_Access;
   --  Returns justification for the argument Rule as it is set by processed
   --  Annotate pragmas.

end Gnatcheck.Diagnoses;
