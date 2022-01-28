------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gnatcheck.Compiler;  use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses; use Gnatcheck.Diagnoses;
with Gnatcheck.Options;   use Gnatcheck.Options;
with Gnatcheck.Output;    use Gnatcheck.Output;
with Gnatcheck.Projects;  use Gnatcheck.Projects;
with Gnatcheck.Projects.Aggregate;
with Gnatcheck.Source_Table; use Gnatcheck.Source_Table;
with Gnatcheck.Rules;     use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Checker_App; use Checker_App;
with Rules_Factory;

procedure Lalcheck is
   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   use type Ada.Calendar.Time;

   Ctx          : Lkql_Context;
   GPRbuild_Pid : Process_Id := Invalid_Pid;

   E_Success   : constant := 0; --  No tool failure, no rule violation detected
   E_Violation : constant := 1; --  No tool failure, rule violation(s) detected
   E_Error     : constant := 2; --  Tool failure detected
   No_Check    : constant := 3; --  No file has been checked

   function File_Name (Id : String; Job : Natural) return String is
     (Global_Report_Dir.all & "gnatcheck-" & Id & Image (Job) & ".TMP");
   --  Return the full path for a temp file with a given Id

   procedure Schedule_Files;
   --  Schedule jobs per set of files

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

      for Rule in All_Rules.First .. All_Rules.Last loop
         if Is_Enabled (All_Rules.Table (Rule).all) then
            Print_Rule_To_File (All_Rules.Table (Rule).all, File);
            New_Line (File);
         end if;
      end loop;

      Close (File);

      Total_Jobs := Num_Jobs +
                      (if Analyze_Compiler_Output then 1 else 0);

      if not Quiet_Mode and not Progress_Indicator_Mode then
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
                  Info ("Error while waiting for gnatcheck process.");
                  return;
               end if;

               Current := @ + 1;

               if Progress_Indicator_Mode then
                  declare
                     Percent : String :=
                       Integer'Image ((Current * 100) / Total_Jobs);
                  begin
                     Percent (1) := '(';
                     Info ("completed" & Integer'Image (Current)
                           & " out of" & Integer'Image (Total_Jobs) & " "
                           & Percent & "%)...");
                  end;
               elsif not Quiet_Mode then
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

                        if not Debug_Mode then
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

         Process_Sources (Ctx, Annotate_Only => True);

         for Job in 1 .. Num_Jobs loop
            Create (File, Out_File, File_Name ("files", Job));
            Files := 0;

            for SF in Next_SF .. Last_Argument_Source loop
               if Source_Info (SF) /= Ignore_Unit then
                  Put_Line (File, Short_Source_Name (SF));
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
              Spawn_Gnatcheck
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

         if not Debug_Mode then
            Delete_File (File_Name ("rules", 0), Status);
         end if;
      end;
   end Schedule_Files;

begin
   Initialize_Option_Scan
     (Stop_At_First_Non_Switch => False,
      Section_Delimiters       => "cargs rules");
   Gnatcheck_Prj.Scan_Arguments (First_Pass => True);

   if Print_Version then
      Print_Tool_Version (2004);
      OS_Exit (E_Success);

   elsif Print_Usage then
      Print_Gnatcheck_Usage;
      OS_Exit (E_Success);
   end if;

   --  If we have the project file specified as a tool parameter, analyze it.

   Gnatcheck.Projects.Process_Project_File (Gnatcheck_Prj);

   --  Create the LKQL context and load the rules

   Ctx := Gnatcheck.Source_Table.Create_Context;
   Process_Rules (Ctx);

   --  Analyze relevant project properties if needed

   if Gnatcheck_Prj.Is_Specified
     and then not Subprocess_Mode
     and then not In_Aggregate_Project
   then
      Extract_Tool_Options (Gnatcheck_Prj);
   end if;

   --  And finally - analyze the command-line parameters.

   Initialize_Option_Scan
     (Stop_At_First_Non_Switch => False,
      Section_Delimiters       => "cargs rules");
   Gnatcheck_Prj.Scan_Arguments;

   Gnatcheck.Projects.Check_Parameters;  --  check that the rule exists

   if not Subprocess_Mode then
      Gnatcheck.Diagnoses.Init_Exemptions;
   end if;

   if Check_Restrictions or else Use_gnatw_Option then
      Create_Restriction_Pragmas_File;
   end if;

   Process_Requested_Rules (Ctx);

   if Nothing_To_Do then
      Gnatcheck.Projects.Clean_Up (Gnatcheck_Prj);
      OS_Exit (No_Check);
   end if;

   if In_Aggregate_Project then
      --  In this case we spawn gnatcheck for each project being aggregated
      Gnatcheck.Projects.Aggregate.Process_Aggregated_Projects (Gnatcheck_Prj);

   elsif Subprocess_Mode then
      --  The call to Create_Context above was made before sources are computed
      --  by Check_Parameters, so reset them now.

      Add_Sources_To_Context (Ctx, Gnatcheck_Prj);
      Process_Sources (Ctx);
      Rules_Factory.Finalize_Rules (Ctx.Eval_Ctx);
      Free_Eval_Context (Ctx.Eval_Ctx);

   else
      --  The call to Create_Context above was made before sources are computed
      --  by Check_Parameters, so reset them now.

      Add_Sources_To_Context (Ctx, Gnatcheck_Prj);

      --  Implement -j via multiple processes
      --  In the default -j1 mode, process all sources in the main process.

      if Process_Num <= 1 then

         --  Spawn gprbuild in background to process the files in parallel

         if Analyze_Compiler_Output then
            GPRbuild_Pid := Spawn_GPRbuild
                              (Global_Report_Dir.all & "gprbuild.err");
         end if;

         Process_Sources (Ctx);

         --  Wait for gprbuild to finish if we've launched it earlier and
         --  analyze its output.

         if Analyze_Compiler_Output then
            declare
               Ignore : Boolean;
               Pid    : Process_Id;
            begin
               if not Quiet_Mode then
                  Info ("Waiting for gprbuild...");
               end if;

               Wait_Process (Pid, Ignore);

               if Pid = GPRbuild_Pid then
                  Analyze_Output (Global_Report_Dir.all & "gprbuild.err",
                                  Ignore);
               else
                  Info ("Error while waiting for gprbuild process.");
               end if;
            end;
         end if;
      else
         Schedule_Files;
      end if;

      Generate_Qualification_Report;
      Gnatcheck.Output.Close_Report_Files;
      Rules_Factory.Finalize_Rules (Ctx.Eval_Ctx);
      Free_Eval_Context (Ctx.Eval_Ctx);

      if Tool_Failures > 0 then
         Info ("Total gnatcheck failures:" & Tool_Failures'Img);
      end if;
   end if;

   Gnatcheck.Projects.Clean_Up (Gnatcheck.Options.Gnatcheck_Prj);

   if Compute_Timing then
      Info ("Execution time:" &
            Duration'Image (Ada.Calendar.Clock - Time_Start));
   end if;

   OS_Exit (if (Detected_Non_Exempted_Violations > 0
                or else Detected_Compiler_Error > 0)
              and then not Brief_Mode
            then E_Violation
            elsif Tool_Failures = 0 and Detected_Internal_Error = 0
            then E_Success
            else E_Error);

exception
   when Parameter_Error =>
      --  The diagnosis is already generated
      Info ("try ""gnatcheck --help"" for more information.");
      OS_Exit (E_Error);

   when Fatal_Error =>
      --  The diagnosis is already generated
      OS_Exit (E_Error);

   when Ex : others =>
      Gnatcheck.Output.Report_Unhandled_Exception (Ex);
      Gnatcheck.Projects.Clean_Up (Gnatcheck_Prj);
      OS_Exit (E_Error);
end Lalcheck;
