------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

   Ctx          : LKQL_Context;
   GPRbuild_Pid : Process_Id;

   E_Success   : constant := 0; --  No tool failure, no rule violation detected
   E_Violation : constant := 1; --  No tool failure, rule violation(s) detected
   E_Error     : constant := 2; --  Tool failure detected
   No_Check    : constant := 3; --  No file has been checked

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

   --  The call to Create_Context above was made before sources are computed
   --  by Check_Parameters, so reset them now.

   Add_Sources_To_Context (Ctx, Gnatcheck_Prj);

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
      Process_Sources (Ctx);
      Rules_Factory.Finalize_Rules (Ctx.Eval_Ctx);
      Free_Eval_Context (Ctx.Eval_Ctx);

   else
      --  Spawn gprbuild in background to process the files in parallel

      if Analyze_Compiler_Output then
         GPRbuild_Pid := Spawn_GPRbuild
                           (Global_Report_Dir.all & "gprbuild.err");
      end if;

      --  Implement -j via multiple processes
      --  In the default -j1 mode, process all sources in the environment
      --  task (Process_Num = 0).

      if Process_Num <= 1 then
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
         declare
            Minimum_Cost : constant := 4;
            Cost         : Natural := 0;
            Cost_Per_Job : Natural;
            Overall_Cost : Natural := 0;
            Costs        : array (1 .. Process_Num) of Natural;
            Pids         : array (1 .. Process_Num) of Process_Id;
            Pid          : Process_Id;
            First        : Boolean;
            File         : Ada.Text_IO.File_Type;
            Status       : Boolean;
            Total_Procs  : Natural;
            Source_File_Name : constant String :=
              Global_Report_Dir.all & "files.txt";

            function File_Name (Id : String; Job : Natural) return String is
              (Global_Report_Dir.all & Id & Image (Job) & ".txt");
            --  Return the full path for a temp file with a given Id

         begin
            --  Compute cost of all rules

            for R in All_Rules.First .. All_Rules.Last loop
               if Is_Enabled (All_Rules.Table (R).all) then
                  Overall_Cost := @ + All_Rules.Table (R).Execution_Cost;
               end if;
            end loop;

            --  Compute average cost per job

            Cost_Per_Job := (Overall_Cost + Process_Num - 1) / Process_Num;

            --  Reduce number of jobs if too few rules

            if Cost_Per_Job < Minimum_Cost then
               Cost_Per_Job := Minimum_Cost;
               Process_Num := (Overall_Cost + Cost_Per_Job - 1) / Cost_Per_Job;
            end if;

            --  Create Source_File_Name with the list of source files to
            --  analyze.

            Create (File, Out_File, Source_File_Name);

            for SF in First_SF_Id .. Last_Argument_Source loop
               if Source_Info (SF) /= Ignore_Unit then
                  Put_Line (File, Short_Source_Name (SF));
               end if;
            end loop;

            Close (File);

            --  Create one rule file per job using the average cost

            for Job in 1 .. Process_Num loop
               Create (File, Out_File, File_Name ("rules", Job));
               Costs (Job) := 0;
               First := True;

               for Rule in All_Rules.First .. All_Rules.Last loop
                  --  Look for active rules, with no job assigned yet and
                  --  either this is the first rule for this job, or this is
                  --  the last job, or we have not exceeded our execution
                  --  budget yet.

                  if Is_Enabled (All_Rules.Table (Rule).all)
                    and then All_Rules.Table (Rule).Job = 0
                    and then (First or else
                              Job = Process_Num or else
                              Costs (Job) +
                                All_Rules.Table (Rule).Execution_Cost
                                  <= Cost_Per_Job)
                  then
                     Costs (Job) := @ + All_Rules.Table (Rule).Execution_Cost;
                     Cost := @ + All_Rules.Table (Rule).Execution_Cost;
                     All_Rules.Table (Rule).Job := Job;
                     Print_Rule_To_File
                       (All_Rules.Table (Rule).all, File);

                     if Debug_Mode then
                        Put (File, " --  cost:" & Costs (Job)'Img);
                     end if;

                     New_Line (File);
                     First := False;

                     exit when Costs (Job) = Cost_Per_Job;

                     --  Adjust Cost_Per_Job in case we exceeded it

                     if Costs (Job) > Cost_Per_Job then
                        Cost_Per_Job := Natural'Max
                          (Minimum_Cost,
                           (Overall_Cost - Cost + Process_Num - Job - 1) /
                            (Process_Num - Job));
                        exit;
                     end if;
                  end if;
               end loop;

               --  If First is True is means we have already scheduled all
               --  rules. Adjust Process_Num and exit the loop.

               if First then
                  Delete (File);
                  Process_Num := Job - 1;
                  exit;
               else
                  Close (File);

                  --  Spawn gnatcheck with --subprocess switch and
                  --  -rules -from=rules?.txt.

                  Pids (Job) :=
                    Spawn_Gnatcheck
                      (File_Name ("rules", Job),
                       File_Name ("gnatcheck-tmp", Job),
                       Source_File_Name);
               end if;
            end loop;

            Total_Procs := Process_Num +
                            (if Analyze_Compiler_Output then 1 else 0);

            if not Quiet_Mode and not Progress_Indicator_Mode then
               Info_No_EOL ("Jobs remaining:");
               Info_No_EOL (Integer'Image (Total_Procs) & ASCII.CR);
            end if;

            --  Wait for all children to finish and process their output

            for J in 1 .. Total_Procs loop
               Wait_Process (Pid, Status);

               if Progress_Indicator_Mode then
                  declare
                     Percent : String :=
                       Integer'Image ((J * 100) / Total_Procs);
                  begin
                     Percent (1) := '(';
                     Info ("completed" & Integer'Image (J) & " out of"
                           & Integer'Image (Total_Procs) & " "
                           & Percent & "%)...");
                  end;
               elsif not Quiet_Mode then
                  Info_No_EOL ("Jobs remaining:");
                  Info_No_EOL (Integer'Image (Total_Procs - J));
                  Info_No_EOL ("     " & ASCII.CR);
               end if;

               if Pid = GPRbuild_Pid then
                  Analyze_Output (Global_Report_Dir.all & "gprbuild.err",
                                  Status);
               else
                  for Job in Pids'Range loop
                     if Pids (Job) = Pid then
                        Analyze_Output (File_Name ("gnatcheck-tmp", Job),
                                        Status);
                        exit;
                     end if;
                  end loop;
               end if;
            end loop;
         end;
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
