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
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gnatcheck.Compiler;  use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses; use Gnatcheck.Diagnoses;
with Gnatcheck.Options;   use Gnatcheck.Options;
with Gnatcheck.Output;    use Gnatcheck.Output;
with Gnatcheck.Projects;  use Gnatcheck.Projects;
with Gnatcheck.Projects.Aggregate;
with Gnatcheck.Source_Table; use Gnatcheck.Source_Table;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

with Checker_App; use Checker_App;
with Rules_Factory;

procedure Lalcheck is
   Time_Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   use type Ada.Calendar.Time;

   Ctx       : LKQL_Context;
   Pid, Pid2 : Process_Id;

   E_Success   : constant := 0; --  No tool failure, no rule violation detected
   E_Violation : constant := 1; --  No tool failure, rule violation(s) detected
   E_Error     : constant := 2; --  Tool failure detected
   No_Check    : constant := 3; --  No file has been checked

   task type LKQL_Task;

   task body LKQL_Task is
      Ctx : LKQL_Context;
   begin
      --  Create the LKQL context and load the rules
      Ctx := Gnatcheck.Source_Table.Create_Context;
      Process_Rules (Ctx);
      Process_Requested_Rules (Ctx);
      Process_Sources (Ctx);
      Rules_Factory.Finalize_Rules (Ctx.Eval_Ctx);
   end LKQL_Task;

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

   if Gnatcheck_Prj.Is_Specified and not In_Aggregate_Project then
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

   Add_Sources_To_Context (Ctx);

   Gnatcheck.Diagnoses.Init_Exemptions;

   if Check_Restrictions or else Use_gnatw_Option then
      Create_Restriction_Pragmas_File;
   end if;

   Process_Requested_Rules (Ctx);

   if Gnatcheck.Options.Nothing_To_Do then
      Gnatcheck.Projects.Clean_Up (Gnatcheck_Prj);
      OS_Exit (No_Check);
   end if;

   if Gnatcheck.Options.In_Aggregate_Project then
      --  In this case we spawn gnatcheck for each project being aggregated
      Gnatcheck.Projects.Aggregate.Process_Aggregated_Projects (Gnatcheck_Prj);
   else
      --  Spawn gprbuild in background to process the files in parallel

      if Analyze_Compiler_Output then
         Pid := Spawn_GPRbuild;
      end if;

      --  Implement -j via multiple tasks
      --  ??? This is not working yet, but the code is provided to help
      --  investigating.
      --  In the default -j1 mode, process all sources in the environment
      --  task (Process_Num = 0).

      declare
         Servers : array (1 .. Process_Num - 1) of LKQL_Task;
         pragma Unreferenced (Servers);
      begin
         Process_Sources (Ctx);
         Rules_Factory.Finalize_Rules (Ctx.Eval_Ctx);
      end;

      --  Wait for gprbuild to finish if we've launched it earlier and analyze
      --  its output.

      if Analyze_Compiler_Output then
         declare
            Ignore : Boolean;
         begin
            if not Quiet_Mode then
               Info ("Waiting for gprbuild...");
            end if;

            Wait_Process (Pid2, Ignore);

            if Pid2 = Pid then
               Analyze_Builder_Output (Ignore);
            else
               Info ("Error while waiting for gprbuild process.");
            end if;
         end;
      end if;

      Generate_Qualification_Report;
      Gnatcheck.Output.Close_Report_Files;

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
            elsif Tool_Failures = 0 then E_Success
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
