------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2004-2023, AdaCore                     --
--                                                                          --
-- GNATCHECK  is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package defines options that are supposed to be of a common interest
--  for all the tools.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with Gnatcheck.Projects;
with Rules_Factory; use Rules_Factory;

package Gnatcheck.Options is

   Gnatcheck_Version : constant String := "23.0w";
   --  Full major.minor version for Gnatcheck

   Date : constant String := "unknown date";

   Version_String : constant String := Gnatcheck_Version & " (" & Date & ")";

   Current_Year : constant String := "2022";

   Executable : constant String := Base_Name (Command_Name);
   --  Name of the current executable (e.g. "gnatcheck" or "gnatkp")

   Custom_Worker_Var : constant String := "GNATCHECK_WORKER";
   --  The name of the environment variable used to define a custom worker
   --  executable.

   Default_Worker : constant String := "lkql gnatcheck_worker";
   --  The name of the worker executable to use.

   Worker_Name : constant String :=
     (if Ada.Environment_Variables.Exists (Custom_Worker_Var)
      then Ada.Environment_Variables.Value (Custom_Worker_Var)
      else Default_Worker);
   --  The name of the worker to use.

   Use_External_Worker : constant Boolean := Worker_Name /= "gnatcheck";
   --  Whether a custom GNATcheck worker executable should be used

   Worker_Command : constant String :=
     (if Use_External_Worker then Worker_Name else Command_Name);
   --  The executable to use as GNATcheck worker

   RTS_Path : GNAT.OS_Lib.String_Access := new String'("");
   --  Runtime as specified via --RTS=

   Target : GNAT.OS_Lib.String_Access := new String'("");
   --  Target as it is specified by the command-line '--target=...' option, or
   --  by the 'Target attribute in the argument project file.

   Charset : GNAT.OS_Lib.String_Access := new String'("iso-8859-1");
   --  Charset to use for parsing source files

   Global_Report_Dir : GNAT.OS_Lib.String_Access := new String'("./");
   --  The name of the directory to place the global results into

   Additional_Rules_Dirs : Path_Vector;
   --  Additional rules directories specified via --rules-dir

   Fatal_Error : exception;
   --  This exception should be raised when there is no sense any more to do
   --  any work in the tool. When raising this exception, one has to generate
   --  the "fatal" diagnostic message.

   Parameter_Error : exception;
   --  Is raised when an error is detected for the parameters supplied for the
   --  tool (includes both wrong parameters and wrong combinations of correct
   --  parameters).

   N_Of_Aggregated_Projects : Natural := 0;
   --  Number of aggregated projects found

   Gnatkp_Mode : Boolean := Executable = "gnatkp";
   --  Whether we are running in KP detection mode
   --  Set either when the executable is called gnatkp or with the debug switch
   --  '-dkp'

   Print_Version : Boolean := False;
   --  '--version'
   --  Print version info and exit

   Print_Usage : Boolean := False;
   --  '--help'
   --  Print usage info and exit

   Verbose_Mode : Boolean := False;
   --  The verbose mode.
   --  '-v'

   Quiet_Mode       : Boolean := False;
   --  Quiet mode, do not emit messages on stderr
   --  '-q'

   Brief_Mode       : Boolean := False;
   --  Brief mode: like quiet mode except that messages are emitted on stderr
   --  '--brief'

   Debug_Mode       : Boolean := False;
   --  Internal debug mode
   --  '-d'

   Progress_Indicator_Mode : Boolean := False;
   --  Generate the output to be used for GPS progress indicator.
   --  '-dd'

   Generate_XML_Help : Boolean := False;
   --  If this file is ON, the tool generates the XML description of the tool
   --  parameters to be used for creating the GUI in GPS.
   --  '-hx'.

   Compute_Timing : Boolean := False;
   --  If this flag is ON, the total execution time (wall clock) of the tool
   --  is computed and printed out.
   --  '-t'

   Check_Semantic : Boolean := False;
   --  If True, run the compiler to check the semantic of each source file.
   --  --check-semantic

   Legacy : Boolean := False;
   --  If True, run in legacy mode, with no support for additional rule files.

   KP_Version : GNAT.OS_Lib.String_Access;
   --  If set, the relevant GNAT version to check when running gnatkp.

   type Warning_Modes is
     (Quiet,  --  all warnings are suppressed
      Short,
      Normal,
      Full);

   Warning_Mode : Warning_Modes := Normal;
   --  Specifies the warning message level
   --  '-w(q|s|n|f)

   Log_Mode : Boolean := False;
   --  Create the log file and duplicate in this file all the messages
   --  generated by a tool.
   --  '-log'

   Exempted_Units : GNAT.OS_Lib.String_Access := null;
   --  '--ignore=<filename>
   --  File containing a list of units to be exempted. (Depending on a tool,
   --  such units either are not processed or the tool does not generate
   --  results for them).

   Aggregated_Project : Boolean := False;
   --  '-A <project_file>
   --  True if this is a tool call spawned from an original tool call with
   --  aggregated project as a parameter. In this mode the tool processes only
   --  one (non-aggregate) project from the projects being aggregated.

   In_Aggregate_Project : Boolean := False;
   --  True if the tool is called for an aggregate project that aggregates more
   --  than one (non-aggregate) project/

   Files_Switch_Used : Boolean := False;
   --  True if the files= switch was used

   Process_Num : Natural := 1;
   --  The maximal number of cores used
   --  -jN

   J_Specified : Boolean := False;
   --  True if the -jN option was given. This is used to distinguish -j0 on a
   --  uniprocessor from no -j switch.

   Ignore_Project_Switches : Boolean := False;
   --  True if --ignore-project-switches was used.
   --  Ignore gnatcheck switches from the project file if set.

   Simple_Project : Boolean := False;
   --  Simple project mode: only use directories from the project file and
   --  filename patterns.
   --  --simple-project

   ----------------------------------------
   -- Flags computed from other settings --
   ----------------------------------------

   --  The flags listed below are not set by some options, but they are
   --  computed from gnatcheck command line and rule options

   Argument_File_Specified : Boolean := False;
   --  Flag indicating if some argument file is specified for the tool call.

   Nothing_To_Do : Boolean := False;
   --  Flag indicating if a tool does not have any real work to do - that is,
   --  that there are some argument files specified, and there is at least one
   --  argument file that exists. A tool should not rely on the default setting
   --  of this file.

   ------------------------------------------------------
   -- options related to program global state analysis --
   ------------------------------------------------------

   Do_Transitive_Closure : Boolean := False;
   --  Flag indicating if the transitive closure of the call graph is needed.

   Main_Subprogram_Name : GNAT.OS_Lib.String_Access;
   --  -main=<name of the main subprogram>
   --  The name of the source file containing the main subprogram. The name
   --  may or may not contain the suffix. This subprogram is called by the
   --  environment task.

   Generate_Rules_Help : Boolean := False;
   --  '-h'
   --  Generate the rules help information (note, that we can do it only after
   --  registering the rules)

   Check_Param_Redefinition : Boolean := False;
   --  '--check-redefinition'
   --  Check if for parametrized rule the rule parameter is defined more than
   --  once (may happen if gnatcheck has several rule files as parameters, or
   --  when a rule is activated both in the command line and in the rule file.

   Active_Rule_Present : Boolean := False;
   --  Flag indicating if the tool has an activated rule to check. It does not
   --  take into account compiler check, use
   --  Gnatcheck.Compiler.Analyze_Compiler_Output to see if any of the compiler
   --  check is active.

   Missing_Rule_File_Detected : Boolean := False;
   --  Parameter of some '-from=...' rule option denotes a file that does not
   --  exist

   Missing_File_Detected : Boolean := False;
   --  Some argument file is not found.

   Bad_Rule_Detected : Boolean := False;
   --  Rule name in +R or -R denotes an unknown rule or some problem with
   --  rule parameter is detected

   Rule_Option_Problem_Detected : Boolean := False;
   --  Any other problem with rule files and/or rule options is detected (bad
   --  format of a rule file, rule redefinition etc.)

   Compiler_Arg_List : GNAT.OS_Lib.Argument_List_Access;
   --  This variable should contain a full list of compilation options to be
   --  passed to gcc.

   --------------------------------------
   -- Controlling the gnatcheck report --
   --------------------------------------

   Short_Report : Boolean := False;
   --  '-s'
   --  Print the short version of the report file.
   --  Only diagnoses are included in the report file.

   Max_Diagnoses : Natural := 500;
   --  '-m'
   --  Maximum number of diagnoses to print out into Stdout. Zero means that
   --  there is no limitation on the number of diagnoses to be printed out into
   --  Stderr.

   Mapping_Mode : Boolean := False;
   --  If this flag is ON, a rule name is added to the text of each diagnosis.

   Subprocess_Mode : Boolean := False;
   --  If this flag is ON, output messages directly and no other output.
   --  This flag implies -q and no report file is generated.

   User_Info_File           : GNAT.OS_Lib.String_Access;
   User_Info_File_Full_Path : GNAT.OS_Lib.String_Access;
   --  --include-file=<filename>
   --  Name of the user-provided text file to be added as the last
   --  section of the report file. If this option is not set, this section is
   --  not created in the report file.

   Individual_Rules_Set        : Boolean := False;
   More_Then_One_Rule_File_Set : Boolean := False;
   --  Flags used to detect if all the rules specified for a given gnatcheck
   --  call, should be set when parsing rule options

   Rule_File_Name : GNAT.OS_Lib.String_Access;
   --  If More_Then_One_Rule_File_Set is OFF and if a rule file has been
   --  processed, keeps the name of this file, otherwise is null.

   LKQL_Rule_File_Name : GNAT.OS_Lib.String_Access;
   --  Contains the name of the LKQL file to use for rule configuration if
   --  one has been provided, otherwise is null.

   Full_Source_Locations : Boolean := False;
   --  '-l'
   --  If this flag is set ON, gnatcheck adds full source locations in the
   --  report file. In case of an entity declared in the expanded generic
   --  code the full location indicates the location of a construct in the
   --  template and then - the location of the corresponding instantiation of
   --  the template (long location chains are used in case of nested
   --  instantiations). Short location shows only the location of the
   --  corresponding construct in the instantiation.

   ---------------------
   -- Project support --
   ---------------------

   Gnatcheck_Prj : aliased Gnatcheck.Projects.Arg_Project_Type;

end Gnatcheck.Options;
