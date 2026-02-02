--
--  Copyright (C) 2005-2026, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This is the top of the Lkql_Checker hierarchy.

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Lkql_Checker.Compiler;         use Lkql_Checker.Compiler;
with Lkql_Checker.Diagnoses;        use Lkql_Checker.Diagnoses;
with Lkql_Checker.Ids;              use Lkql_Checker.Ids;
with Lkql_Checker.Options;          use Lkql_Checker.Options;
with Lkql_Checker.Output;           use Lkql_Checker.Output;
with Lkql_Checker.Projects;         use Lkql_Checker.Projects;
with Lkql_Checker.Projects.Aggregate;
with Lkql_Checker.Rules;            use Lkql_Checker.Rules;
with Lkql_Checker.Rules.Rule_Table; use Lkql_Checker.Rules.Rule_Table;
with Lkql_Checker.Source_Table;     use Lkql_Checker.Source_Table;
with Lkql_Checker.String_Utilities; use Lkql_Checker.String_Utilities;

package body Lkql_Checker is
   ----------------
   -- Exit codes --
   ----------------

   E_Success : constant := 0;
   --  No tool failure, no rule violation detected

   E_Violation : constant := 1;
   --  No tool failure, rule violation(s) detected

   E_Error : constant := 2;
   --  Tool failure detected

   E_Missing_Source : constant := 3;
   --  Missing at least one argument source

   E_Missing_Rule_File : constant := 4;
   --  Missing coding standard file

   E_Missing_Rule : constant := 5;
   --  Bad rule name or bad rule parameter

   E_Bad_Rules : constant := 6;
   --  Other problem with rules options

   ----------------------
   -- Util subprograms --
   ----------------------

   procedure Setup_Search_Paths;
   --  Initialize LKQL_PATH to include path to built-in rules.
   --  Assuming this executable is in $PREFIX/bin, this includes the
   --  $PREFIX/share/lkql and $PREFIX/share/lkql/kp paths. That way, only
   --  the driver needs to be aware of built-in rules, and worker can be
   --  located anywhere. This also extends dynamic library search paths
   --  (PATH for Windows and LD_LIBRARY_PATH on Linux) to point on
   --  $PREFIX/lib and $PREFIX/lib/libadalang, to allow worker processes
   --  that rely on dynamic libraries to find their dependencies without
   --  requiring users to explicitly set these paths.

   procedure Print_LKQL_Rules
     (File                   : File_Type;
      Mode                   : Source_Modes;
      Include_Compiler_Rules : Boolean := False;
      For_Worker             : Boolean := True);
   --  Print the rule configuration of the given source mode into the given
   --  file using the LKQL rule config file format.
   --  If ``Include_Compiler_Rules`` is True, the compiler based rules
   --  options is included in the emitted LKQL config.
   --  ``For_Worker`` gives the information whether this procedure should
   --  emit an LKQL file formatted for the worker.

   procedure Schedule_Files;
   --  Schedule jobs per set of files

   --------------------------
   --  Lkql_Checker_Image  --
   --------------------------

   function Lkql_Checker_Mode_Image return String
   is (Lkql_Checker_Mode_Name (Mode));

   ------------------------------
   --  Lkql_Checker_Mode_Name  --
   ------------------------------

   function Lkql_Checker_Mode_Name (Mode : Lkql_Checker_Mode) return String is
   begin
      return
        (case Mode is
           when Gnatcheck_Mode => "gnatcheck",
           when Gnatkp_Mode    => "gnatkp");
   end Lkql_Checker_Mode_Name;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Id : String; Job : Natural) return String
   is (Global_Report_Dir.all & "gnatcheck-" & Id & Image (Job) & ".TMP");
   --  Return the full path for a temp file with a given Id

   ------------------------------------
   -- Default_LKQL_Rule_Options_File --
   ------------------------------------

   function Default_LKQL_Rule_Options_File return String
   is (Checker_Prj.Get_Project_Relative_File ("rules.lkql"));
   --  Get the default LKQL rule options file name.

   ------------------------
   -- Setup_Search_Paths --
   ------------------------

   procedure Setup_Search_Paths is
      use Ada;
      use Ada.Directories;

      procedure Add_Path (Env_Var : String; Path : String);
      --  Add the given Path to the given environment variable, using the OS
      --  path separator. If the variable does not exist yet, this defines
      --  it.

      procedure Add_Path (Env_Var : String; Path : String) is
      begin
         if Environment_Variables.Exists (Env_Var) then
            Environment_Variables.Set
              (Env_Var,
               Path & Path_Separator & Environment_Variables.Value (Env_Var));
         else
            Environment_Variables.Set (Env_Var, Path);
         end if;
      end Add_Path;

      Executable : String_Access :=
        Locate_Exec_On_Path (Ada.Command_Line.Command_Name);
   begin
      if Executable = null then
         raise Program_Error
           with "cannot locate " & Ada.Command_Line.Command_Name;
      end if;

      declare
         Prefix : constant String :=
           Containing_Directory (Containing_Directory (Executable.all));

         Lkql : constant String := Compose (Compose (Prefix, "share"), "lkql");
         Kp   : constant String := Compose (Lkql, "kp");

         Lib     : constant String := Compose (Prefix, "lib");
         Lib_LAL : constant String := Compose (Lib, "libadalang");
      begin
         Add_Path ("LKQL_PATH", Lkql);
         Add_Path ("LKQL_PATH", Kp);

         Add_Path ("PATH", Lib);
         Add_Path ("PATH", Lib_LAL);

         Add_Path ("LD_LIBRARY_PATH", Lib);
         Add_Path ("LD_LIBRARY_PATH", Lib_LAL);

         for Path of Additional_Lkql_Paths loop
            Add_Path ("LKQL_PATH", Path);
         end loop;
      end;

      Free (Executable);
   end Setup_Search_Paths;

   ----------------------
   -- Print_LKQL_Rules --
   ----------------------

   procedure Print_LKQL_Rules
     (File                   : File_Type;
      Mode                   : Source_Modes;
      Include_Compiler_Rules : Boolean := False;
      For_Worker             : Boolean := True)
   is
      First : Boolean := True;
   begin
      --  Always emit the "rules" object, otherwise the LKQL config file is
      --  invalid.
      if Mode = General then
         Put_Line (File, "val rules = @{");
      end if;

      --  Write the configuration of all rules instance
      for Rule in All_Rules.Iterate loop
         if Is_Enabled (All_Rules (Rule)) then
            Print_Rule_Instances_To_LKQL_File
              (All_Rules (Rule), File, Mode, For_Worker, First);
         end if;
      end loop;

      --  If required, emit the configuration of the compiler based rules
      if Include_Compiler_Rules and then Mode = General then
         Print_Compiler_Rule_To_LKQL_File (Style_Checks_Id, File, First);
         Print_Compiler_Rule_To_LKQL_File (Warnings_Id, File, First);
         Print_Compiler_Rule_To_LKQL_File (Restrictions_Id, File, First);
      end if;

      --  Close the rule config object
      if not First or Mode = General then
         New_Line (File);
         Put_Line (File, "}");
      end if;
   end Print_LKQL_Rules;

   --------------------
   -- Schedule_Files --
   --------------------

   procedure Schedule_Files is
      Minimum_Files : constant := 10;
      Num_Files     : Natural := 0;
      Num_Jobs      : Natural := 0;
      Current       : Natural := 0;
      Files_Per_Job : Natural;
      File          : Ada.Text_IO.File_Type;
      Status        : Boolean;
      Total_Jobs    : Natural;
      Process_Num   : Natural := Tool_Args.Jobs.Get;
      GPRbuild_Pid  : Process_Id := Invalid_Pid;
   begin
      --  Compute number of files

      for SF in First_SF_Id .. Last_Argument_Source loop
         if Source_Info (SF) /= Ignore_Unit then
            Num_Files := @ + 1;
         end if;
      end loop;

      Files_Per_Job := (Num_Files + Process_Num - 1) / Process_Num;
      Num_Jobs := Process_Num;

      if Files_Per_Job >= 2 * Minimum_Files then
         Files_Per_Job := (@ + 1) / 2;
         Num_Jobs := @ * 2;

      --  Reduce number of jobs if too few files

      elsif Files_Per_Job < Minimum_Files then
         Files_Per_Job := Minimum_Files;
         Process_Num := (Num_Files + Files_Per_Job - 1) / Files_Per_Job;
         Num_Jobs := Process_Num;
      end if;

      --  Create the rules file

      Create (File, Out_File, File_Name ("rules", 0));

      Print_LKQL_Rules (File, General);
      Print_LKQL_Rules (File, Ada_Only);
      Print_LKQL_Rules (File, Spark_Only);

      Close (File);

      Total_Jobs := Num_Jobs + (if Analyze_Compiler_Output then 1 else 0);

      if not Tool_Args.Quiet_Mode and not Tool_Args.Progress_Indicator_Mode.Get
      then
         Print
           ("Jobs remaining:" & Integer'Image (Total_Jobs) & ASCII.CR,
            New_Line    => False,
            Log_Message => False);
      end if;

      --  Process each job with all rules and a different subset of files

      declare
         Pids    : array (1 .. Num_Jobs) of Process_Id;
         Pid     : Process_Id;
         Next_SF : SF_Id := First_SF_Id;
         Files   : Natural;

         procedure Wait_Lkql_Checker;
         --  Wait for one Lkql_Checker child process to finish and
         --  analyze its output. Also deal with the gprbuild child if any.

         -----------------------
         -- Wait_Lkql_Checker --
         -----------------------

         procedure Wait_Lkql_Checker is
            Process_Found : Boolean := False;
         begin
            loop
               Wait_Process (Pid, Status);

               if Pid = Invalid_Pid then
                  --  We still want to set Current to avoid going into an
                  --  infinite loop in Schedule_Files: if we are there, it
                  --  means there are no more processes to wait for.

                  Current := Total_Jobs;
                  Tool_Failures := @ + 1;
                  Error
                    ("error while waiting for gnatcheck process, output "
                     & "may be incomplete.");
                  return;
               end if;

               Current := @ + 1;

               if Tool_Args.Progress_Indicator_Mode.Get then
                  declare
                     Percent : String :=
                       Integer'Image ((Current * 100) / Total_Jobs);
                  begin
                     Percent (1) := '(';
                     Print
                       ("completed"
                        & Integer'Image (Current)
                        & " out of"
                        & Integer'Image (Total_Jobs)
                        & " "
                        & Percent
                        & "%)...",
                        Log_Message => False);
                  end;
               elsif not Tool_Args.Quiet_Mode then
                  Print
                    (Message     =>
                       "Jobs remaining:"
                       & Integer'Image (Total_Jobs - Current)
                       & "     "
                       & ASCII.CR,
                     New_Line    => False,
                     Log_Message => False);
               end if;

               if Pid = GPRbuild_Pid then
                  Analyze_Output
                    (Global_Report_Dir.all & "gprbuild.err", Status);
                  exit when Current = Total_Jobs;

               else
                  for Job in Pids'Range loop
                     if Pids (Job) = Pid then
                        Analyze_Output (File_Name ("out", Job), Status);
                        Process_Found := True;

                        if not Tool_Args.Debug_Mode.Get then
                           Delete_File (File_Name ("out", Job), Status);
                           Delete_File (File_Name ("files", Job), Status);
                        end if;

                        exit;
                     end if;
                  end loop;

                  if not Process_Found then
                     Error ("error while waiting for gprbuild process.");
                  end if;

                  exit;
               end if;
            end loop;
         end Wait_Lkql_Checker;

      begin
         --  Process sources to take pragma Annotate into account

         Process_Sources;

         for Job in 1 .. Num_Jobs loop
            Create (File, Out_File, File_Name ("files", Job));
            Files := 0;

            for SF in Next_SF .. Last_Argument_Source loop
               if Source_Info (SF) /= Ignore_Unit then
                  Put_Line (File, Source_Name (SF));
                  Files := @ + 1;
                  Set_Source_Status (SF, Processed);

                  if Files = Files_Per_Job or else SF = Last_Argument_Source
                  then
                     Next_SF := SF + 1;
                     exit;
                  end if;
               end if;
            end loop;

            Close (File);

            --  Spawn a GNATcheck worker with -rules -from=rules0.txt
            --  -files=files?.txt

            Pids (Job) :=
              Spawn_Checker_Worker
                (File_Name ("rules", 0),
                 File_Name ("out", Job),
                 File_Name ("files", Job),
                 File_Name ("log", Job));

            if Next_SF > Last_Argument_Source then
               Total_Jobs := @ - Num_Jobs + Job;
               exit;
            end if;

            if Job >= Process_Num then
               Wait_Lkql_Checker;
            end if;
         end loop;

         --  Now that some processes are free, spawn gprbuild in background

         if Analyze_Compiler_Output then
            GPRbuild_Pid :=
              Spawn_GPRbuild (Global_Report_Dir.all & "gprbuild.err");
         end if;

         --  Wait for remaining children

         while Current /= Total_Jobs loop
            Wait_Lkql_Checker;
         end loop;

         if not Tool_Args.Debug_Mode.Get then
            Delete_File (File_Name ("rules", 0), Status);
         end if;
      end;
   end Schedule_Files;

   -----------
   --  Main --
   -----------
   procedure Main (Mode : Lkql_Checker_Mode) is
      Time_Start        : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Project_File_Args : String_Vector;
      Remaining_Args    : XString_Vector;

      procedure Close_Log_File;

      procedure Close_Log_File is
      begin
         if GPR_Args.Log_Enabled then
            Lkql_Checker.Output.Close_Log_File;
         end if;
      end Close_Log_File;

      function To_XString_Array (Vec : XString_Vector) return XString_Array;
      function To_XString_Array (Vec : String_Vector) return XString_Array;

      function To_XString_Array (Vec : XString_Vector) return XString_Array is
         Res : XString_Array (Vec.First_Index .. Vec.Last_Index);
      begin
         for I in Res'Range loop
            Res (I) := Vec (I);
         end loop;
         return Res;
      end To_XString_Array;

      function To_XString_Array (Vec : String_Vector) return XString_Array is
         Res : XString_Array (Vec.First_Index .. Vec.Last_Index);
      begin
         for I in Res'Range loop
            Res (I) := To_XString (Vec (I));
         end loop;
         return Res;
      end To_XString_Array;

      use type Ada.Calendar.Time;
      use Ada.Strings.Unbounded;
   begin
      --  Set the Lkql_Checker global mode
      Lkql_Checker.Mode := Mode;

      --  Register GNATcheck GPR attributes
      Register_Tool_Attributes (Checker_Prj);

      --  In a first time, we parse the GPR related switches from the
      --  command-line in order to create the project instance we're going to
      --  use for this run.
      if not GPR_Args.Parser.Parse (No_Arguments, Remaining_Args) then
         raise Parameter_Error;
      end if;

      --  Print help or version if required
      if GPR_Args.Version.Get then
         Print_Version_Info;
         OS_Exit (E_Success);
      elsif GPR_Args.Help.Get then
         Print_Usage;
         OS_Exit (E_Success);
      end if;

      --  Process the project file
      Checker_Prj.Load_Project (GPR_Args.GPR2_Parser.Parsed_GPR2_Options);

      --  Then we fetch tool specific switches from the project file if
      --  required.
      if Checker_Prj.Is_Specified
        and then not In_Aggregate_Project
        and then not GPR_Args.Ignore_Project_Switches
      then
         Project_File_Args := Checker_Prj.Extract_Tool_Options;
      end if;

      --  Open the log file if required
      if GPR_Args.Log_Enabled then
         Open_Log_File;
      end if;

      --  If requested, print the GPR registry and exit
      Checker_Prj.Print_GPR_Registry;

      --  Scan tool specific switches coming from the project file before
      --  scanning the ones from the command-line to be able to override them.
      Scan_Tool_Arguments
        (Args              => To_XString_Array (Project_File_Args),
         From_Project_File => True);

      --  Finally, we parse remaining switches from the command-line with the
      --  GNATcheck arguments parser.
      Scan_Tool_Arguments
        (Args              => To_XString_Array (Remaining_Args),
         From_Project_File => False);

      --  Fetch all sources from the loaded project
      if Aggregate.Num_Of_Aggregated_Projects > 1 then
         if not Main_Unit.Is_Empty then
            Error
              ("'-U main' cannot be used if aggregate project "
               & "aggregates more than one non-aggregate project");

            raise Parameter_Error;
         end if;

         --  No information is extracted from the aggregate project
         --  itself.
         In_Aggregate_Project := True;
      else
         Checker_Prj.Get_Sources_From_Project;
      end if;

      --  Add the command-line rules to the rule options. Do this before the
      --  second argument parsing to avoid duplicate rule names coming from the
      --  command-line.
      for Rule of Tool_Args.Rules.Get loop
         Add_Rule_By_Name (To_String (Rule), Prepend => True);
      end loop;

      --  Add the command-line LKQL_PATH elements to the vector of additional
      --  searching paths.
      for Working_Dir_Path of Tool_Args.Lkql_Path.Get loop
         Additional_Lkql_Paths.Append
           (Normalize_Pathname (To_String (Working_Dir_Path)));
      end loop;

      --  Store legacy rule options before re-parsing the command-line
      if Tool_Args.Has_Legacy_Rule_Options then
         --  Process the legacy rule options section
         Process_Legacy_Rule_Options (Tool_Args.Legacy_Rules_Section.Get);

         --  Emit the depreciation message
         Info_In_Tty
           ("The '-rules' section is now deprecated. You should only use the "
            & "'--rule' and '--rule-file' command-line options.");
         Info_In_Tty
           ("You can use the '--emit-lkql-rule-file' flag to automatically "
            & "translate your rule configuration to the new LKQL format.");
      end if;

      --  Process the source list file if there is one
      if Tool_Args.Source_Files.Get /= Null_Unbounded_String then
         Read_Args_From_File (To_String (Tool_Args.Source_Files.Get));
      end if;

      --  Process the include file
      if Tool_Args.Include_File.Get /= Null_Unbounded_String then
         Lkql_Checker.Diagnoses.Process_User_Filename
           (To_String (Tool_Args.Include_File.Get));
      end if;

      --  Set up ignore list
      if Tool_Args.Ignore_Files.Get /= Null_Unbounded_String then
         if Is_Regular_File (To_String (Tool_Args.Ignore_Files.Get)) then
            Exempted_Units :=
              new String'
                (Normalize_Pathname (To_String (Tool_Args.Ignore_Files.Get)));
         else
            Error (To_String (Tool_Args.Ignore_Files.Get) & " not found");
            raise Parameter_Error;
         end if;
      end if;

      --  Add the command-line LKQL rule file to the rule options
      if Tool_Args.Rule_File.Get /= Null_Unbounded_String then
         Set_LKQL_Rule_File (To_String (Tool_Args.Rule_File.Get), False);
      end if;

      --  Get the default LKQL rule file if none has been specified
      if Is_Rule_Options_Empty and then Checker_Prj.Is_Specified then
         declare
            Def_LKQL : constant String := Default_LKQL_Rule_Options_File;
         begin
            if Is_Regular_File (Def_LKQL) then
               --  The default rule file is already resolved relatively to the
               --  current project file.
               Set_LKQL_Rule_File (Def_LKQL, False);
            end if;
         end;
      end if;

      --  Setup LKQL_PATH to point on built-in rules
      Setup_Search_Paths;

      --  Load rule files after having parsed --rules-dir
      Process_Rules;

      --  And process all rule options after rule files have been loaded
      Process_Rule_Options;

      --  Force some switches and perform some checks for gnatkp
      if Mode = Gnatkp_Mode then
         if Checker_Prj.Target = "" or else Checker_Prj.Runtime = "" then
            Error ("missing explicit target and/or runtime");
            OS_Exit (E_Error);
         end if;
      end if;

      Lkql_Checker.Projects.Check_Parameters;  --  check that the rule exists

      if Analyze_Compiler_Output then
         Create_Restriction_Pragmas_File;
      end if;

      --  If required, export the current rule configuration to the
      --  'rules.lkql' file.
      if Tool_Args.Emit_LKQL_Rule_File.Get then
         --  Ensure that the 'rules.lkql' file doesn't exists
         if Is_Regular_File (Default_LKQL_Rule_Options_File) then
            Error
              ("cannot emit the LKQL rule file, "
               & Default_LKQL_Rule_Options_File
               & " already exists");
            OS_Exit (E_Error);
         end if;

         declare
            File : Ada.Text_IO.File_Type;
         begin
            Create (File, Out_File, Default_LKQL_Rule_Options_File);

            Print_LKQL_Rules
              (File                   => File,
               Mode                   => General,
               Include_Compiler_Rules => True,
               For_Worker             => False);
            Print_LKQL_Rules (File, Ada_Only, For_Worker => True);
            Print_LKQL_Rules (File, Spark_Only, For_Worker => True);

            Close (File);
         end;
      end if;

      if No_Detectors_For_KP_Version then
         Lkql_Checker.Projects.Clean_Up (Checker_Prj);
         OS_Exit (E_Success);
      elsif Nothing_To_Do then
         Lkql_Checker.Projects.Clean_Up (Checker_Prj);
         OS_Exit (E_Missing_Source);
      end if;

      if In_Aggregate_Project then
         --  In this case we spawn gnatcheck for each project being aggregated
         Lkql_Checker.Projects.Aggregate.Process_Aggregated_Projects
           (Checker_Prj);

      else
         --  Implement -j via multiple processes. In the default (-j1, no
         --  custom worker) mode, process all sources in the main process.
         Schedule_Files;

         Generate_Qualification_Report;
         Lkql_Checker.Output.Close_Report_Files;

         if Tool_Failures > 0 then
            Print ("Total gnatcheck failures:" & Tool_Failures'Img);
         end if;
      end if;

      Lkql_Checker.Projects.Clean_Up (Lkql_Checker.Options.Checker_Prj);

      if Tool_Args.Time.Get then
         Print
           ("Execution time:"
            & Duration'Image (Ada.Calendar.Clock - Time_Start));
      end if;

      Lkql_Checker.Rules.Rule_Table.Clean_Up;

      --  Close the log file if required
      Close_Log_File;

      OS_Exit
        (if Tool_Failures /= 0
           or else Detected_Internal_Error /= 0
           or else Error_From_Warning
         then E_Error
         elsif Missing_Rule_File_Detected
         then E_Missing_Rule_File
         elsif Bad_Rule_Detected
         then E_Missing_Rule
         elsif Rule_Option_Problem_Detected
         then E_Bad_Rules
         elsif Missing_File_Detected
         then E_Missing_Source

         --  If we are here, no problem with gnatcheck execution or rule
         --  option or missing file definition is detected, so we can trust
         --  gnatcheck results.

         elsif (Detected_Non_Exempted_Violations > 0
                or else Detected_Compiler_Error > 0)
           and then not Tool_Args.Brief_Mode
         then E_Violation
         else E_Success);

   exception
      when Parameter_Error =>
         --  The diagnosis is already generated
         Print
           ("try """
            & Lkql_Checker_Mode_Image
            & " --help"" for more information.",
            Log_Message => False);

         --  Close the log file if required
         Close_Log_File;

         OS_Exit (E_Error);

      when Fatal_Error =>
         --  The diagnosis is already generated
         OS_Exit (E_Error);

      when Ex : others =>
         Lkql_Checker.Output.Report_Unhandled_Exception (Ex);
         Lkql_Checker.Projects.Clean_Up (Checker_Prj);

         --  Close the log file if required
         Close_Log_File;

         OS_Exit (E_Error);
   end Main;
end Lkql_Checker;
