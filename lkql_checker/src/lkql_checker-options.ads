--
--  Copyright (C) 2005-2026, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines interfaces that provide support to process and store
--  input for the GNATcheck tool.

with Ada.Containers.Vectors;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GPR2.Options.Opt_Parse;
with Lkql_Checker.Projects;
with Lkql_Checker.String_Utilities; use Lkql_Checker.String_Utilities;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with GPR2.Options;

package Lkql_Checker.Options is

   Lkql_Checker_Version : constant String := "27.0w";
   --  Full major.minor version for Lkql_Checker

   Date : constant String := "unknown date";

   Current_Year : constant String := "2025";

   Version_String : constant String :=
     Lkql_Checker_Version & " (" & Date & ")";

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

   Active_Rule_Present : Boolean := False;
   --  Flag indicating if the tool has an activated rule to check. It does not
   --  take into account compiler check, use
   --  Lkql_Checker.Compiler.Analyze_Compiler_Output to see if any of the
   --  compiler check is active.

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

   Additional_Lkql_Paths : String_Vector;
   --  Additional paths to add to the ``LKQL_PATH`` environment variable when
   --  spawning the LKQL worker.

   Instance_Help_Emitted : Boolean := False;
   --  Whether the help message about the new instance system has already been
   --  emitted. This message should be removed in 26.0.

   --------------------------------------
   -- Controlling the gnatcheck report --
   --------------------------------------

   User_Info_File           : GNAT.OS_Lib.String_Access;
   User_Info_File_Full_Path : GNAT.OS_Lib.String_Access;
   --  --include-file=<filename>
   --  Name of the user-provided text file to be added as the last
   --  section of the report file. If this option is not set, this section is
   --  not created in the report file.

   Individual_Rules_Set : Boolean := False;
   --  Flags used to detect if all the rules specified for a given gnatcheck
   --  call, should be set when parsing rule options.

   Legacy_Rule_File_Name : Unbounded_String := Null_Unbounded_String;
   --  If only one legacy rule file has been provided, store it here.

   LKQL_Rule_File_Name : Unbounded_String := Null_Unbounded_String;
   --  Name of the LKQL file to process as a rule file. We assume that the
   --  stored value is an absolute path to the LKQL rule file.

   ---------------------
   -- Project support --
   ---------------------

   Checker_Prj : aliased Lkql_Checker.Projects.Arg_Project_Type;

   ---------------------------
   -- Opt_Parse integration --
   ---------------------------

   type Lkql_Checker_Error_Handler is new Error_Handler with null record;
   subtype Max_Diagnoses_Count is Natural range 0 .. 1000;

   procedure Warning (Self : in out Lkql_Checker_Error_Handler; Msg : String);
   procedure Error (Self : in out Lkql_Checker_Error_Handler; Msg : String);

   function Jobs_Convert (Arg : String) return Natural;
   function Project_Verbosity_Convert (Arg : String) return Natural;
   function Max_Diagnoses_Convert (Arg : String) return Max_Diagnoses_Count;

   function Is_New_Section (Arg : XString) return Boolean;

   package Arg is
      Parser : Argument_Parser :=
        Create_Argument_Parser
          (Help                 => "GNATcheck help",
           Incremental          => True,
           Generate_Help_Flag   => False,
           Custom_Error_Handler =>
             Create (Lkql_Checker_Error_Handler'(null record)),
           Print_Help_On_Error  => False);

      package GPR_Args is new GPR2.Options.Opt_Parse.Args (Parser);

      package Version is new
        Parse_Flag
          (Parser => Parser,
           Name   => "Version",
           Long   => "--version",
           Short  => "-V",
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

      package Cargs_Section is new
        Parse_Option_List
          (Parser              => Parser,
           Long                => "-cargs",
           Name                => "Compiler options",
           Accumulate          => True,
           Arg_Number          => Multiple_Args,
           Allow_Empty         => True,
           Arg_Type            => Unbounded_String,
           List_Stop_Predicate => Is_New_Section,
           Help                => "options forwarded to the compiler",
           Legacy_Long_Form    => True);

      package Legacy_Rules_Section is new
        Parse_Option_List
          (Parser              => Parser,
           Long                => "-rules",
           Name                => "Rule options",
           Accumulate          => True,
           Arg_Number          => Multiple_Args,
           Allow_Empty         => True,
           Arg_Type            => XString,
           List_Stop_Predicate => Is_New_Section,
           Help                => "legacy rule options",
           Legacy_Long_Form    => True);

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
      is (Ignore_Project_Switches_Opt.Get or Mode = Gnatkp_Mode);

      function Source_Files_Specified return Boolean
      is (Source_Files.Get /= Null_Unbounded_String);

      function Has_Legacy_Rule_Options return Boolean
      is (Legacy_Rules_Section.Get'Length > 0);

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
      is (Resolve_Report_File
            (Text_Output.Get, Lkql_Checker_Mode_Image & ".out"));

      function XML_Report_File_Path return String
      is (Resolve_Report_File
            (XML_Output.Get, Lkql_Checker_Mode_Image & ".xml"));
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

   ------------------
   -- Rule options --
   ------------------

   type Option_Kind is (File, Legacy_Option, Single_Rule_Name);

   type Option_Record is record
      Kind  : Option_Kind;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Vector_Options is new
     Ada.Containers.Vectors (Positive, Option_Record);

   Rule_Options : Vector_Options.Vector;

   procedure Process_Rule_Options;
   --  Process all the rule options found as part of scanning arguments.

   procedure Process_Legacy_Rule_Options
     (Args : Arg.Legacy_Rules_Section.Result_Array);
   --  Process the options provided in ``Args`` as legacy rule options.

   procedure Add_Legacy_Rule_Option (Opt : String; Prepend : Boolean := False);
   --  Add the given ``Opt`` to the list of rule options processed by
   --  ``Process_Rule_Options`` as a command-line rule option (e.g. +R...).
   --  If ``Prepend`` is set to True, add the rule option at the start of
   --  the processing list.

   procedure Add_Rule_By_Name (Rule_Name : String; Prepend : Boolean := False);
   --  Create a new rule option to enable the rule designated by the provided
   --  name without any additional configuration.

   procedure Set_LKQL_Rule_File (File : String; Project_Relative : Boolean);
   --  Set the given ``File`` as the LKQL rule file to process during the
   --  execution of ``Process_Rule_Options``. If a rule file has already been
   --  set, this function displays an error and set the
   --  ``Rule_Option_Problem_Detected`` flag to True.
   --  If the provided ``File`` isn't an absolute path, if ``Project_Relative``
   --  is set to ``True``, resolve the provided file relatively to
   --  the current project file (if any). Else, resolve ``File`` relatively to
   --  the current working directory.

   function Is_Rule_Options_Empty return Boolean;
   --  Get whether the rule options are empty.

   --------------------------------
   -- Legacy rule options parser --
   --------------------------------

   package Legacy_Rule_Options is
      Parser : Argument_Parser :=
        Create_Argument_Parser
          (Help                => "Legacy rule options parser",
           Generate_Help_Flag  => False,
           Print_Help_On_Error => False);

      package From_Files is new
        Parse_Option_List
          (Parser           => Parser,
           Long             => "-from",
           Arg_Type         => Unbounded_String,
           Accumulate       => True,
           Legacy_Long_Form => True);
   end Legacy_Rule_Options;

end Lkql_Checker.Options;
