--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines options that are supposed to be of a common interest
--  for all the tools.

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Directories; use Ada.Directories;
with Ada.Environment_Variables;

with GNAT.OS_Lib;

with Gnatcheck.Projects;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

package Gnatcheck.Options is

   Gnatcheck_Version : constant String := "25.0w";
   --  Full major.minor version for Gnatcheck

   Date : constant String := "unknown date";

   Current_Year : constant String := "2024";

   Version_String : constant String := Gnatcheck_Version & " (" & Date & ")";

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

   RTS_Path : GNAT.OS_Lib.String_Access := new String'("");
   --  Runtime as specified via --RTS= or Runtime attribute

   Target : GNAT.OS_Lib.String_Access := new String'("");
   --  Target as it is specified by the command-line '--target=...' option, or
   --  by the 'Target attribute in the argument project file.

   Global_Report_Dir : GNAT.OS_Lib.String_Access := new String'("./");
   --  The name of the directory to place the global results into

   Fatal_Error : exception;
   --  This exception should be raised when there is no sense any more to do
   --  any work in the tool. When raising this exception, one has to generate
   --  the "fatal" diagnostic message.

   Parameter_Error : exception;
   --  Is raised when an error is detected for the parameters supplied for the
   --  tool (includes both wrong parameters and wrong combinations of correct
   --  parameters).

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

   No_Detectors_For_KP_Version : Boolean := False;
   --  Flag indicating that GNATkp was launched with a --kp-version argument
   --  corresponding to a GNAT version for which there are no known problems
   --  (and hence no matching rule detector). In that case, we consider the
   --  run to be successful. NOTE that this only applies to the case where the
   --  given version is valid, otherwise it must be considered an error and
   --  this flag should not be set.

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

   Max_Diagnoses : Natural := 0;
   --  '-m'
   --  Maximum number of diagnoses to print out into Stdout. Zero means that
   --  there is no limitation on the number of diagnoses to be printed out into
   --  Stderr.

   Mapping_Mode : Boolean := False;
   --  If this flag is ON, a rule name is added to the text of each diagnosis.

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

   package Arg is
      Parser : Argument_Parser := Create_Argument_Parser
        (Help               => "GNATcheck help",
         Incremental        => True,
         Generate_Help_Flag => False);

      package Check_Semantic is new Parse_Flag
        (Parser => Parser,
         Long   => "--check-semantic",
         Help   => "check semantic validity of the source files");

      package No_Subprojects is new Parse_Flag
        (Parser => Parser,
         Long   => "--no-subprojects",
         Help   => "process only sources of root project");

      package Transitive_Closure is new Parse_Flag
        (Parser => Parser,
         Short  => "-U",
         Name   => "Closure",
         Help   => "process all units of the closure rooted in the mains "
                   & "passed as arguments (or mains of the project if list "
                   & "is empty)");

      package Charset is new Parse_Option
        (Parser      => Parser,
         Long        => "--charset",
         Arg_Type    => Unbounded_String,
         Default_Val => To_Unbounded_String ("iso-8859-1"),
         Help        => "specify the charset of the source files (default is "
                        & "latin-1)");

      package Rules_Dirs is new Parse_Option_List
        (Parser     => Parser,
         Long       => "--rules-dir",
         Arg_Type   => Unbounded_String,
         Accumulate => True,
         Enabled    => not Legacy,
         Help       => "specify an alternate directory containing rule files");

      package Project_File is new Parse_Option
        (Parser      => Parser,
         Short       => "-P",
         Long        => "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "project file to use");

      --  TODO: This needs to be private (undocumented)
      package Aggregate_Subproject is new Parse_Option
        (Parser      => Parser,
         Short       => "-A",
         Name        => "Aggregate project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "private flag - used when processing a subproject of "
                        & "a root aggregate project");

      function Aggregated_Project return Boolean
      is (Aggregate_Subproject.Get /= Null_Unbounded_String);

   end Arg;

end Gnatcheck.Options;
