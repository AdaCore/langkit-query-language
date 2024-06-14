--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Checker_App;

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses;        use Gnatcheck.Diagnoses;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Projects;         use Gnatcheck.Projects;
with Gnatcheck.Projects.Aggregate;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Source_Table;     use Gnatcheck.Source_Table;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with GPR2.Project.Registry.Exchange;

procedure Gnatcheck_Main is
   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   use type Ada.Calendar.Time;

   Ctx          : Checker_App.Lkql_Context;
   GPRbuild_Pid : Process_Id := Invalid_Pid;

   E_Success   : constant := 0; --  No tool failure, no rule violation detected
   E_Violation : constant := 1; --  No tool failure, rule violation(s) detected
   E_Error     : constant := 2; --  Tool failure detected

   E_Missing_Source : constant := 3; --  Missing at least one argument source

   --  Exit code for problems with rule specifications
   E_Missing_Rule_File : constant := 4; --  Missing coding standard file
   E_Missing_Rule      : constant := 5; --  Bad rule name or bad rule parameter
   E_Bad_Rules         : constant := 6; --  Other problem with rules options

   function File_Name (Id : String; Job : Natural) return String is
     (Global_Report_Dir.all & "gnatcheck-" & Id & Image (Job) & ".TMP");
   --  Return the full path for a temp file with a given Id

   procedure Setup_Search_Paths;
   --  Initialize LKQL_RULES_PATH to include path to built-in rules. Assuming
   --  this executable is in $PREFIX/bin, this includes the $PREFIX/share/lkql
   --  and $PREFIX/share/lkql/kp paths. That way, only the driver needs to be
   --  aware of built-in rules, and worker can be located anywhere.
   --  This also extends dynamic library search paths (PATH for Windows and
   --  LD_LIBRARY_PATH on Linux) to point on $PREFIX/lib and
   --  $PREFIX/lib/libadalang, to allow worker processes that rely on dynamic
   --  libraries to find their dependencies without requiring users to
   --  explicitly set these paths.

   procedure Print_LKQL_Rules (File : File_Type; Mode : Source_Modes);
   --  Print the rule configuration of the given source mode into the given
   --  file using the LKQL rule config file format.

   procedure Schedule_Files;
   --  Schedule jobs per set of files

   ------------------------
   -- Setup_Search_Paths --
   ------------------------

   procedure Setup_Search_Paths is
      use Ada;
      use Ada.Directories;

      procedure Add_Path (Env_Var : String; Path : String);
      --  Add the given Path to the given environment variable, using the OS
      --  path separator. If the variable does not exist yet, this defines it.

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

      Executable : String_Access := Locate_Exec_On_Path
        (Ada.Command_Line.Command_Name);
   begin
      if Executable = null then
         raise Program_Error with
            "cannot locate " & Ada.Command_Line.Command_Name;
      end if;

      declare
         Prefix : constant String :=
            Containing_Directory (Containing_Directory (Executable.all));

         Lkql : constant String := Compose (Compose (Prefix, "share"), "lkql");
         Kp   : constant String := Compose (Lkql, "kp");

         Lib     : constant String := Compose (Prefix, "lib");
         Lib_LAL : constant String := Compose (Lib, "libadalang");
      begin
         Add_Path ("LKQL_RULES_PATH", Lkql);
         Add_Path ("LKQL_RULES_PATH", Kp);

         Add_Path ("PATH", Lib);
         Add_Path ("PATH", Lib_LAL);

         Add_Path ("LD_LIBRARY_PATH", Lib);
         Add_Path ("LD_LIBRARY_PATH", Lib_LAL);
      end;

      Free (Executable);
   end Setup_Search_Paths;

   ----------------------
   -- Print_LKQL_Rules --
   ----------------------

   procedure Print_LKQL_Rules (File : File_Type; Mode : Source_Modes) is
      Mode_String : constant String :=
        (case Mode is
            when General    => "rules",
            when Ada_Only   => "ada_rules",
            when Spark_Only => "spark_rules");

      First : Boolean := True;
   begin
      Put_Line (File, "val " & Mode_String & " = @{");
      for Rule in All_Rules.Iterate loop
         if Is_Enabled (All_Rules (Rule)) then
            Print_Rule_Instances_To_LKQL_File
              (All_Rules (Rule), File, Mode, First);
         end if;
      end loop;
      New_Line (File);
      Put_Line (File, "}");
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

      Total_Jobs := Num_Jobs +
                      (if Analyze_Compiler_Output then 1 else 0);

      if not Arg.Quiet_Mode and not Arg.Progress_Indicator_Mode.Get then
         Info_No_EOL ("Jobs remaining:");
         Info_No_EOL (Integer'Image (Total_Jobs) & ASCII.CR);
      end if;

      --  Process each job with all rules and a different subset of files

      declare
         Pids    : array (1 .. Num_Jobs) of Process_Id;
         Pid     : Process_Id;
         Next_SF : SF_Id := First_SF_Id;
         Files   : Natural;

         procedure Wait_Gnatcheck;
         --  Wait for one gnatcheck child process to finish and
         --  analyze its output. Also deal with the gprbuild child if any.

         --------------------
         -- Wait_Gnatcheck --
         --------------------

         procedure Wait_Gnatcheck is
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
                  Info ("Error while waiting for gnatcheck process, output " &
                        "may be incomplete.");
                  return;
               end if;

               Current := @ + 1;

               if Arg.Progress_Indicator_Mode.Get then
                  declare
                     Percent : String :=
                       Integer'Image ((Current * 100) / Total_Jobs);
                  begin
                     Percent (1) := '(';
                     Info ("completed" & Integer'Image (Current)
                           & " out of" & Integer'Image (Total_Jobs) & " "
                           & Percent & "%)...");
                  end;
               elsif not Arg.Quiet_Mode then
                  Info_No_EOL ("Jobs remaining:");
                  Info_No_EOL (Integer'Image (Total_Jobs - Current));
                  Info_No_EOL ("     " & ASCII.CR);
               end if;

               if Pid = GPRbuild_Pid then
                  Analyze_Output (Global_Report_Dir.all & "gprbuild.err",
                                  Status);
                  exit when Current = Total_Jobs;

               else
                  for Job in Pids'Range loop
                     if Pids (Job) = Pid then
                        Analyze_Output (File_Name ("out", Job), Status);
                        Process_Found := True;

                        if not Arg.Debug_Mode.Get then
                           Delete_File (File_Name ("out", Job), Status);
                           Delete_File (File_Name ("files", Job), Status);
                        end if;

                        exit;
                     end if;
                  end loop;

                  if not Process_Found then
                     Info ("Error while waiting for gprbuild process.");
                  end if;

                  exit;
               end if;
            end loop;
         end Wait_Gnatcheck;

      begin
         --  Process sources to take pragma Annotate into account

         Process_Sources (Ctx);

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

            --  Spawn gnatcheck with --subprocess switch and
            --  -rules -from=rules0.txt -files=files?.txt

            Pids (Job) :=
              Spawn_Gnatcheck_Worker
                (File_Name ("rules", 0),
                 File_Name ("out", Job),
                 File_Name ("files", Job));

            if Next_SF > Last_Argument_Source then
               Total_Jobs := @ - Num_Jobs + Job;
               exit;
            end if;

            if Job >= Process_Num then
               Wait_Gnatcheck;
            end if;
         end loop;

         --  Now that some processes are free, spawn gprbuild in background

         if Analyze_Compiler_Output then
            GPRbuild_Pid := Spawn_GPRbuild
                              (Global_Report_Dir.all & "gprbuild.err");
         end if;

         --  Wait for remaining children

         while Current /= Total_Jobs loop
            Wait_Gnatcheck;
         end loop;

         if not Arg.Debug_Mode.Get then
            Delete_File (File_Name ("rules", 0), Status);
         end if;
      end;
   end Schedule_Files;

   use Ada.Strings.Unbounded;
begin
   Initialize_Environment;

   Gnatcheck_Prj.Scan_Arguments (First_Pass => True);

   if Print_Version then
      Print_Tool_Version (2004);
      OS_Exit (E_Success);

   elsif Print_Usage then
      Print_Gnatcheck_Usage;
      OS_Exit (E_Success);
   end if;

   --  Store project file
   if Arg.Project_File.Get /= Null_Unbounded_String then
      Gnatcheck_Prj.Store_Project_Source
        (To_String (Arg.Project_File.Get));
   end if;

   --  Store aggregate subproject file
   if Arg.Aggregate_Subproject.Get /= Null_Unbounded_String then
      Gnatcheck.Projects.Aggregate.Store_Aggregated_Project
        (To_String (Arg.Aggregate_Subproject.Get));
   end if;

   --  Store scenario variables
   for Var of Arg.Scenario_Vars.Get loop
      Store_External_Variable (To_String (Var));
   end loop;

   --  Store .cgpr
   if Arg.Config_File.Get /= Null_Unbounded_String then
      Gnatcheck_Prj.Store_CGPR_Source (To_String (Arg.Config_File.Get));
   end if;

   --  Store target from project file
   Gnatcheck.Options.Target := Arg.Target.Get;

   --  Store target from project file
   Gnatcheck.Options.RTS_Path := Arg.RTS.Get;

   --  Register GNATcheck GPR attributes

   Register_Tool_Attributes (Gnatcheck_Prj);

   --  Print GPR registered and exit if requested

   if Print_Gpr_Registry then
      --  Print GPR registry

      GPR2.Project.Registry.Exchange.Export (Output => Put'Access);
      OS_Exit (E_Success);
   end if;

   Gnatcheck.Projects.Set_Default_Target;

   --  If we have the project file specified as a tool parameter, analyze it.

   Gnatcheck.Projects.Process_Project_File (Gnatcheck_Prj);

   --  Create the LKQL context

   Ctx := Gnatcheck.Source_Table.Create_Context;

   --  Ignore project switches when running as gnatkp

   if Gnatkp_Mode then
      Ignore_Project_Switches := True;
   end if;

   --  Analyze relevant project properties if needed

   if Gnatcheck_Prj.Is_Specified
     and then not In_Aggregate_Project
     and then not Ignore_Project_Switches
   then
      Extract_Tool_Options (Gnatcheck_Prj);
   end if;

   --  Then analyze the command-line parameters

   Gnatcheck_Prj.Scan_Arguments;

   --  Setup LKQL_RULES_PATH to point on built-in rules

   Setup_Search_Paths;

   --  Load rule files after having parsed --rules-dir

   Process_Rules (Ctx);

   --  And process all rule options after rule files have been loaded

   Process_Rule_Options;

   --  Force some switches for gnatkp

   if Gnatkp_Mode then
      Max_Diagnoses  := 0;
      Log_Mode       := False;
   end if;

   Set_Log_File;
   Gnatcheck.Projects.Check_Parameters;  --  check that the rule exists

   if Analyze_Compiler_Output then
      Create_Restriction_Pragmas_File;
   end if;

   if No_Detectors_For_KP_Version then
      Gnatcheck.Projects.Clean_Up (Gnatcheck_Prj);
      OS_Exit (E_Success);
   elsif Nothing_To_Do then
      Gnatcheck.Projects.Clean_Up (Gnatcheck_Prj);
      OS_Exit (E_Missing_Source);
   end if;

   if In_Aggregate_Project then
      --  In this case we spawn gnatcheck for each project being aggregated
      Gnatcheck.Projects.Aggregate.Process_Aggregated_Projects (Gnatcheck_Prj);

   else
      --  The call to Create_Context above was made before sources are computed
      --  by Check_Parameters, so reset them now.

      Add_Sources_To_Context (Ctx, Gnatcheck_Prj);

      --  Implement -j via multiple processes
      --  In the default (-j1, no custom worker) mode, process all sources in
      --  the main process.

      Schedule_Files;

      Generate_Qualification_Report;
      Gnatcheck.Output.Close_Report_Files;

      if Tool_Failures > 0 then
         Info ("Total gnatcheck failures:" & Tool_Failures'Img);
      end if;
   end if;

   Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);

   if Arg.Time.Get then
      Info ("Execution time:" &
            Duration'Image (Ada.Calendar.Clock - Time_Start));
   end if;

   Gnatcheck.Rules.Rule_Table.Clean_Up;
   Close_Log_File;

   OS_Exit (if Tool_Failures /= 0
              or else
               Detected_Internal_Error /= 0
            then                                    E_Error
            elsif Missing_Rule_File_Detected   then E_Missing_Rule_File
            elsif Bad_Rule_Detected            then E_Missing_Rule
            elsif Rule_Option_Problem_Detected then E_Bad_Rules
            elsif Missing_File_Detected        then E_Missing_Source

            --  If we are here, no problem with gnatcheck execution or rule
            --  option or missing file definition is detected, so we can trust
            --  gnatcheck results.

            elsif (Detected_Non_Exempted_Violations > 0
                or else Detected_Compiler_Error > 0)
                 and then not Arg.Brief_Mode
            then E_Violation
            else E_Success);

exception
   when Parameter_Error =>
      --  The diagnosis is already generated
      Info ("try """ & Executable & " --help"" for more information.");
      OS_Exit (E_Error);

   when Fatal_Error =>
      --  The diagnosis is already generated
      OS_Exit (E_Error);

   when Ex : others =>
      Gnatcheck.Output.Report_Unhandled_Exception (Ex);
      Gnatcheck.Projects.Clean_Up (Gnatcheck_Prj);
      OS_Exit (E_Error);
end Gnatcheck_Main;
