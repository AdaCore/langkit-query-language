--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines options that are supposed to be of a common interest
--  for all the tools.

with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Gnatcheck.Projects;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with GPR2.Options;

package Gnatcheck.Options is

   Gnatcheck_Version : constant String := "26.0w";
   --  Full major.minor version for Gnatcheck

   Date : constant String := "unknown date";

   Current_Year : constant String := "2025";

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

   RTS_Path : Unbounded_String := Null_Unbounded_String;
   --  Runtime as specified via --RTS= or Runtime attribute

   Target : Unbounded_String := Null_Unbounded_String;
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

   Legacy : Boolean := False;
   --  If True, run in legacy mode, with no support for additional rule files.

   Exempted_Units : GNAT.OS_Lib.String_Access := null;
   --  '--ignore=<filename>
   --  File containing a list of units to be exempted. (Depending on a tool,
   --  such units either are not processed or the tool does not generate
   --  results for them).

   In_Aggregate_Project : Boolean := False;
   --  True if the tool is called for an aggregate project that aggregates more
   --  than one (non-aggregate) project/

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

   Compiler_Arg_List : Argument_List_Access;
   --  This variable should contain a full list of compilation options to be
   --  passed to gcc.

   Additional_Lkql_Paths : String_Vector;
   --  Additional paths to add to the ``LKQL_PATH`` environment variable when
   --  spawning the LKQL worker.

   Instance_Help_Emitted : Boolean := False;
   --  Whether the help message about the new instance system has already been
   --  emitted. This message should be removed in 26.0.

   Rules_Depreciation_Emitted : Boolean := False;
   --  Whether the message about the ``-rules`` section depreciation has been
   --  emitted in the TTY.

   --------------------------------------
   -- Controlling the gnatcheck report --
   --------------------------------------

   User_Info_File           : GNAT.OS_Lib.String_Access;
   User_Info_File_Full_Path : GNAT.OS_Lib.String_Access;
   --  --include-file=<filename>
   --  Name of the user-provided text file to be added as the last
   --  section of the report file. If this option is not set, this section is
   --  not created in the report file.

   Individual_Rules_Set               : Boolean := False;
   More_Than_One_Legacy_Rule_File_Set : Boolean := False;
   --  Flags used to detect if all the rules specified for a given gnatcheck
   --  call, should be set when parsing rule options

   Legacy_Rule_File_Name : GNAT.OS_Lib.String_Access;
   --  If ``More_Than_One_Legacy_Rule_File_Set`` is OFF and if a rule file has
   --  been processed, keeps the name of this file, otherwise is null.

   LKQL_Rule_File_Name : Unbounded_String := Null_Unbounded_String;
   --  Name of the LKQL file to process as a rule file. We assume that the
   --  stored value is an absolute path to the LKQL rule file.

   ---------------------
   -- Project support --
   ---------------------

   Gnatcheck_Prj : aliased Gnatcheck.Projects.Arg_Project_Type;

   ---------------------------
   -- Opt_Parse integration --
   ---------------------------

   type Gnatcheck_Error_Handler is new Error_Handler with null record;
   subtype Max_Diagnoses_Count is Natural range 0 .. 1000;

   procedure Warning (Self : in out Gnatcheck_Error_Handler; Msg : String);
   procedure Error (Self : in out Gnatcheck_Error_Handler; Msg : String);

   function Jobs_Convert (Arg : String) return Natural;
   function Project_Verbosity_Convert (Arg : String) return Natural;
   function Max_Diagnoses_Convert (Arg : String) return Max_Diagnoses_Count;

   package Arg is
      Parser : Argument_Parser :=
        Create_Argument_Parser
          (Help                 => "GNATcheck help",
           Incremental          => True,
           Generate_Help_Flag   => False,
           Custom_Error_Handler =>
             Create (Gnatcheck_Error_Handler'(null record)),
           Print_Help_On_Error  => False);

      package Version is new
        Parse_Flag
          (Parser => Parser,
           Name   => "Version",
           Long   => "--version",
           Help   => "show the tool version and exit");

      package Help is new
        Parse_Flag
          (Parser => Parser,
           Name   => "Help",
           Long   => "--help",
           Short  => "-h",
           Help   => "show the help message and exit");

      package List_Rules is new
        Parse_Flag
          (Parser => Parser,
           Name   => "List rules",
           Long   => "--list-rules",
           Help   => "show the list of predefined rules and exit");

      package List_Rules_XML is new
        Parse_Flag
          (Parser           => Parser,
           Name             => "List rules XML",
           Long             => "-hx",
           Help             =>
             "show the list of predefined rules formatted in XML and exit",
           Legacy_Long_Form => True);

      package Check_Semantic is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--check-semantic",
           Help   => "check semantic validity of the source files");

      package No_Subprojects is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--no-subprojects",
           Help   => "process only sources of root project");

      package Transitive_Closure is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-U",
           Name   => "Closure",
           Help   =>
             "process all units of the closure rooted in the mains "
             & "passed as arguments (or mains of the project if list "
             & "is empty)");

      package Charset is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--charset",
           Arg_Type    => Unbounded_String,
           Default_Val => To_Unbounded_String ("iso-8859-1"),
           Help        =>
             "specify the charset of the source files (default is "
             & "latin-1)");

      package KP_Version is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--kp-version",
           Name        => "KP version",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "enable all KP detectors matching GNAT <version>");

      package Lkql_Path is new
        Parse_Option_List
          (Parser     => Parser,
           Long       => "--lkql-path",
           Arg_Type   => Unbounded_String,
           Accumulate => True,
           Help       =>
             "specify directories to add to the 'LKQL_PATH' environment "
             & "variable when spawning the LKQL worker");

      package Rules_Dirs is new
        Parse_Option_List
          (Parser     => Parser,
           Long       => "--rules-dir",
           Arg_Type   => Unbounded_String,
           Accumulate => True,
           Enabled    => not Legacy,
           Help       =>
             "specify an alternate directory containing rule files");

      package Project_File is new
        Parse_Option
          (Parser      => Parser,
           Short       => "-P",
           Long        => "--project",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "project file to use");

      package Scenario_Vars is new
        Parse_Option_List
          (Parser     => Parser,
           Short      => "-X",
           Name       => "Scenario variable",
           Arg_Type   => Unbounded_String,
           Accumulate => True,
           Help       => "scenario variables to pass to the project file");

      package Project_Verbosity is new
        Parse_Option
          (Parser           => Parser,
           Long             => "-vP",
           Name             => "Project verbosity",
           Legacy_Long_Form => True,
           Arg_Type         => Natural,
           Default_Val      => 0,
           Convert          => Project_Verbosity_Convert,
           Help             =>
             "verbosity level when parsing a project file (from 0 to 2, "
             & "default is 0)");

      package Config_File is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--config",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "name of the configuration project file. If passed, "
             & "this file must exist and neither --target nor --RTS"
             & "must be passed.");

      package Target is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--target",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "name of the target to use when loading the project");

      package RTS is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--RTS",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "name of the runtime (RTS) to use when loading the " & "project");

      package Full_Source_Locations is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-l",
           Name   => "Full source locations",
           Help   => "full pathname for file locations");

      package Debug_Mode is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-d",
           Name   => "Debug mode",
           Help   => "activate debug mode");

      package Progress_Indicator_Mode is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-dd",
           Name   => "Progress indicator mode",
           Help   => "activate progress indicator mode");

      --  TODO: This needs to be private (undocumented)
      package Aggregate_Subproject is new
        Parse_Option
          (Parser      => Parser,
           Short       => "-A",
           Name        => "Aggregate project",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "private flag - used when processing a subproject of "
             & "a root aggregate project");

      package Follow_Symbolic_Links is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-eL",
           Name   => "Follow symbolic links",
           Help   =>
             "follow all symbolic links when processing project files");

      function Aggregated_Project return Boolean
      is (Aggregate_Subproject.Get /= Null_Unbounded_String);

      package Quiet is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-q",
           Name   => "Quiet mode",
           Help   => "quiet mode, do not emit messages on stderr");

      package Verbose is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-v",
           Long   => "--verbose",
           Name   => "Verbose mode",
           Help   => "enable the verbose mode");

      package Log is new
        Parse_Flag
          (Parser           => Parser,
           Long             => "-log",
           Name             => "Log mode",
           Legacy_Long_Form => True,
           Help             =>
             "duplicate all messages sent to stderr in gnatcheck.log");

      package Max_Diagnoses is new
        Parse_Option
          (Parser      => Parser,
           Short       => "-m",
           Name        => "Max diagnoses",
           Arg_Type    => Max_Diagnoses_Count,
           Default_Val => 0,
           Convert     => Max_Diagnoses_Convert,
           Help        =>
             "set the maximal number of diagnoses in stderr (0 for all "
             & "diagnoses, default is 0)");

      package Brief is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--brief",
           Help   =>
             "brief mode: like quiet mode except that messages are "
             & "emitted on stderr");

      package Short is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-s",
           Name   => "Short report",
           Help   => "print the short version of the report file");

      package No_Text_Report is new
        Parse_Flag
          (Parser           => Parser,
           Long             => "-nt",
           Name             => "No text report",
           Legacy_Long_Form => True,
           Help             =>
             "do not generate text report (enforces '-xml')");

      package XML_Report is new
        Parse_Flag
          (Parser           => Parser,
           Long             => "-xml",
           Name             => "XML report",
           Legacy_Long_Form => True,
           Help             => "generate report in XML format");

      package Text_Output is new
        Parse_Option
          (Parser                    => Parser,
           Short                     => "-o",
           Name                      => "Text output",
           Arg_Type                  => Unbounded_String,
           Default_Val               => Null_Unbounded_String,
           Allow_Collated_Short_Form => False,
           Help                      =>
             "specify the name of the text report file");

      package XML_Output is new
        Parse_Option
          (Parser           => Parser,
           Long             => "-ox",
           Name             => "XML output",
           Arg_Type         => Unbounded_String,
           Default_Val      => Null_Unbounded_String,
           Legacy_Long_Form => True,
           Help             =>
             "specify the name of the XML report file (enforces '-xml')");

      package Time is new
        Parse_Flag
          (Parser => Parser,
           Short  => "-t",
           Name   => "Compute timing",
           Help   => "print the total execution time (wall clock) on stderr");

      package Show_Rule is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--show-rule",
           Help   => "append rule names to diagnoses generated");

      package Show_Instantiation_Chain is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--show-instantiation-chain",
           Help   =>
             "show instantiation chain for reported generic construct");

      package Check_Redefinition is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--check-redefinition",
           Help   => "issue warning if a rule parameter is redefined");

      package No_Object_Dir is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--no_objects_dir",
           Help   => "issue warning if a rule parameter is redefined");

      package Print_Gpr_Registry is new
        Parse_Flag
          (Parser => Parser,
           Long   => GPR2.Options.Print_GPR_Registry_Option,
           Help   => "TODO");

      package Ignore_Project_Switches_Opt is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--ignore-project-switches",
           Help   => "ignore switches specified in the project file");

      package Include_File is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--include-file",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "add the content of filename into generated report");

      package Subdirs is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--subdirs",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        =>
             "specify subdirectory to place the result files into");

      package Source_Files is new
        Parse_Option
          (Parser           => Parser,
           Long             => "-files",
           Name             => "Source files",
           Arg_Type         => Unbounded_String,
           Default_Val      => Null_Unbounded_String,
           Legacy_Long_Form => True,
           Help             =>
             "the name of the text file containing a list of Ada source "
             & "files to analyze");

      package Ignore_Files is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--ignore",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "do not process sources listed in filename");

      package Jobs is new
        Parse_Option
          (Parser      => Parser,
           Short       => "-j",
           Name        => "Jobs",
           Arg_Type    => Natural,
           Default_Val => 1,
           Convert     => Jobs_Convert,
           Help        => "the maximal number of processes");

      package Rules is new
        Parse_Option_List
          (Parser                    => Parser,
           Short                     => "-r",
           Long                      => "--rule",
           Arg_Type                  => Unbounded_String,
           Accumulate                => True,
           Allow_Collated_Short_Form => False,
           Help                      =>
             "enable the given rules for the GNATcheck run");

      package Rule_File is new
        Parse_Option
          (Parser      => Parser,
           Long        => "--rule-file",
           Arg_Type    => Unbounded_String,
           Default_Val => Null_Unbounded_String,
           Help        => "read rule configuration from the given LKQL file");

      package Emit_LKQL_Rule_File is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--emit-lkql-rule-file",
           Help   =>
             "emit a 'rules.lkql' file containing the rules "
             & "configuration");

      package Warnings_As_Errors is new
        Parse_Flag
          (Parser => Parser,
           Long   => "--warnings-as-errors",
           Short  => "-W",
           Help   => "Treat warning messages as errors");

      ----------------------
      -- Option shortcuts --
      ----------------------

      function Quiet_Mode return Boolean
      is (Quiet.Get or else Brief.Get);

      function Short_Report return Boolean
      is (Brief.Get or else Short.Get);

      function Brief_Mode return Boolean
      is (Brief.Get);

      function Text_Report_Enabled return Boolean
      is (not No_Text_Report.Get);

      function Text_Report_File_Path return String;

      function XML_Report_Enabled return Boolean
      is (No_Text_Report.Get
          or else XML_Report.Get
          or else XML_Output.Get /= Null_Unbounded_String);

      function XML_Report_File_Path return String;

      function Ignore_Project_Switches return Boolean
      is (Ignore_Project_Switches_Opt.Get or Gnatkp_Mode);

      function Source_Files_Specified return Boolean
      is (Source_Files.Get /= Null_Unbounded_String);

   private
      function Resolve_Report_File
        (File_Name : Unbounded_String; Default_Name : String) return String
      is (declare
            Report_File_Name : constant String :=
              (if File_Name = Null_Unbounded_String
               then Default_Name
               else To_String (File_Name));
          begin
            Normalize_Pathname
              (if Is_Absolute_Path (Report_File_Name)
               then Report_File_Name
               else Global_Report_Dir.all & Report_File_Name));
      --  Resolve the provided ``File_Name`` relatively to the
      --  ``Global_Report_Dir`` if it is not already an absolute path. If the
      --  provided file name is a ``Null_Unbounded_String``, then fallback on
      --  the specified ``Default_Name``.

      function Text_Report_File_Path return String
      is (Resolve_Report_File (Text_Output.Get, Executable & ".out"));

      function XML_Report_File_Path return String
      is (Resolve_Report_File (XML_Output.Get, Executable & ".xml"));
   end Arg;

   -----------------------
   -- Option processing --
   -----------------------

   procedure Scan_Arguments
     (First_Pass : Boolean := False; Args : Argument_List_Access := null);
   --  Process GNATcheck CLI arguments. If provided ``Args`` is not null, this
   --  procedure assumes that arguments are coming from a project file
   --  ``Switches`` or ``Default_Switches`` attribute. Otherwise, arguments are
   --  fetched from the application's command-line.
   --  The ``First_Pass`` parameter indicates whether this is the first time
   --  arguments are processed. This is used to avoid processing same arguments
   --  multiple times.

end Gnatcheck.Options;
