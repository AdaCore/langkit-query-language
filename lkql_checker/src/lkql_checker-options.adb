with GNAT.Command_Line; use GNAT.Command_Line;

with Lkql_Checker.Output;       use Lkql_Checker.Output;
with Lkql_Checker.Projects;     use Lkql_Checker.Projects;
with Lkql_Checker.Source_Table; use Lkql_Checker.Source_Table;

with System.Multiprocessors;

package body Lkql_Checker.Options is

   ------------------------------
   -- Opt_Parse error handling --
   ------------------------------

   procedure Warning (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Warning (Msg);
   end Warning;

   procedure Error (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Error (Msg);
   end Error;

   -------------------
   -- Local helpers --
   -------------------

   function Parse_Arg_As_Natural (Arg : String) return Natural;
   --  Parse the provided string as a ``Natural`` value, raising an
   --  ``Opt_Parse_Error`` in case of parsing error.

   function Parse_Arg_As_Natural (Arg : String) return Natural is
   begin
      begin
         return Natural'Value (Arg);
      exception
         when Constraint_Error =>
            raise Opt_Parse_Error with "cannot parse provided value: " & Arg;
      end;
   end Parse_Arg_As_Natural;

   ------------------
   -- Jobs_Convert --
   ------------------

   function Jobs_Convert (Arg : String) return Natural is
      Value : constant Natural := Parse_Arg_As_Natural (Arg);
   begin
      if Value = 0 then
         return Natural (System.Multiprocessors.Number_Of_CPUs);
      else
         return Value;
      end if;
   end Jobs_Convert;

   -------------------------------
   -- Project_Verbosity_Convert --
   -------------------------------

   function Project_Verbosity_Convert (Arg : String) return Natural is
      Value : constant Natural := Parse_Arg_As_Natural (Arg);
   begin
      if Value > 2 then
         raise Opt_Parse_Error with "invalid value: " & Arg;
      else
         return Value;
      end if;
   end Project_Verbosity_Convert;

   ---------------------------
   -- Max_Diagnoses_Convert --
   ---------------------------

   function Max_Diagnoses_Convert (Arg : String) return Max_Diagnoses_Count is
   begin
      begin
         return Max_Diagnoses_Count'Value (Arg);
      exception
         when Constraint_Error =>
            raise Opt_Parse_Error
              with
                "invalid maximum diagnoses value: " & Arg & " (max is 1000)";
      end;
   end Max_Diagnoses_Convert;

   --------------------
   -- Is_New_Section --
   --------------------

   function Is_New_Section (Arg : XString) return Boolean is
   begin
      return Arg = "-rules" or else Arg = "-cargs";
   end Is_New_Section;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (First_Pass : Boolean := False; Args : Argument_List_Access := null)
   is
      Unknown_Opt_Parse_Args : XString_Vector;
      Exp_It                 : Expansion_Iterator;
      Explicit_Sources       : String_Vector;

      function To_XString_Array
        (Args : Argument_List_Access) return XString_Array;

      function To_XString_Array
        (Args : Argument_List_Access) return XString_Array
      is
         Ret : XString_Array (Args'Range);
      begin
         for I in Args'Range loop
            Ret (I) := To_XString (Args (I).all);
         end loop;
         return Ret;
      end To_XString_Array;

      Executable : GNAT.OS_Lib.String_Access :=
        Locate_Exec_On_Path (Command_Name);
      Prefix     : constant String :=
        Containing_Directory (Containing_Directory (Executable.all));
      Lkql       : constant String :=
        Compose (Compose (Prefix, "share"), "lkql");

      Args_From_Project : constant Boolean := Args /= null;

      --  Start of processing for Scan_Arguments

   begin
      --  Set Legacy early so that this flag can be checked elsewhere.
      --  If legacy-rules.txt is found, it means we have a full packaging
      --  and a full gnatcheck. If the file is not found, it means we have
      --  a reduced packaging and a legacy gnatcheck.

      if not Is_Regular_File (Compose (Lkql, "legacy-rules.txt")) then
         Legacy := True;
      end if;

      Free (Executable);

      --  Disallow arguments that are not allowed to be specified in project
      --  files
      --  TODO: It might be possible to have a list of subparsers and do a for
      --  loop
      if Args_From_Project then
         declare
            In_Project_Msg : constant String :=
              " is forbidden in project file";
         begin
            Disallow (Arg.Aggregate_Subproject.This, "-A" & In_Project_Msg);
            Disallow (Arg.Project_File.This, "-P" & In_Project_Msg);
            Disallow (Arg.Transitive_Closure.This, "-U" & In_Project_Msg);
            Disallow (Arg.Scenario_Vars.This, "-Xname=val" & In_Project_Msg);
            Disallow
              (Arg.No_Subprojects.This, "--no-subprojects" & In_Project_Msg);
            Disallow (Arg.Project_Verbosity.This, "-vP" & In_Project_Msg);
            Disallow (Arg.Follow_Symbolic_Links.This, "-eL" & In_Project_Msg);
            Disallow (Arg.Lkql_Path.This, "--lkql-path" & In_Project_Msg);
            Disallow (Arg.Rules.This, "-r" & In_Project_Msg);
            Disallow (Arg.Rule_File.This, "--rule-file" & In_Project_Msg);
            Disallow (Arg.Target.This, "--target" & In_Project_Msg);
            Disallow (Arg.RTS.This, "--RTS" & In_Project_Msg);
            Disallow (Arg.Version.This, "--version" & In_Project_Msg);
            Disallow (Arg.Help.This, "-h, --help" & In_Project_Msg);
            Disallow (Arg.List_Rules.This, "--list-rules" & In_Project_Msg);
         end;
      end if;

      if not Arg.Parser.Parse
               ((if Args /= null
                 then To_XString_Array (Args)
                 else No_Arguments),
                Unknown_Arguments => Unknown_Opt_Parse_Args)
      then
         raise Parameter_Error;
      end if;

      --  Reallow arguments that were disallowed
      if Args_From_Project then
         Allow (Arg.Transitive_Closure.This);
         Allow (Arg.Scenario_Vars.This);
         Allow (Arg.Aggregate_Subproject.This);
         Allow (Arg.Project_File.This);
         Allow (Arg.No_Subprojects.This);
         Allow (Arg.Project_Verbosity.This);
         Allow (Arg.Follow_Symbolic_Links.This);
         Allow (Arg.Lkql_Path.This);
         Allow (Arg.Rules.This);
         Allow (Arg.Rule_File.This);
         Allow (Arg.Target.This);
         Allow (Arg.RTS.This);
         Allow (Arg.Version.This);
         Allow (Arg.Help.This);
         Allow (Arg.List_Rules.This);
      end if;

      --  Now that we processed all switches, remaining arguments should be
      --  source files to analyze.
      --  We first have to check that there is no additional argument and
      --  expand possible glob patterns.
      for Unknown_Arg of Unknown_Opt_Parse_Args loop
         --  We consider that arguments starting by "-" are remaining unknown
         --  arguments.
         if Unknown_Arg.Starts_With ("-") then
            Error ("unrecognized switch: " & To_String (Unknown_Arg));
            raise Parameter_Error;

         --  We now know that the current argument should be handled like an
         --  Ada source file name OR a glob pattern.
         --  We only handle explicit sources during the first pass to avoid
         --  duplication.
         elsif Args_From_Project or First_Pass then
            declare
               Is_Glob : constant Boolean :=
                 Unknown_Arg.Find ('*') /= 0
                 or else Unknown_Arg.Find ('?') /= 0
                 or else Unknown_Arg.Find ('[') /= 0;
            begin
               if Is_Glob then
                  Start_Expansion (Exp_It, To_String (Unknown_Arg));
                  loop
                     declare
                        Expanded_Arg : constant String := Expansion (Exp_It);
                     begin
                        exit when Expanded_Arg = "";
                        Explicit_Sources.Append (Expanded_Arg);
                     end;
                  end loop;
               else
                  Explicit_Sources.Append (To_String (Unknown_Arg));
               end if;
            end;
         end if;
      end loop;

      --  We can now store sources to process
      for Arg of Explicit_Sources loop
         if Options.Arg.Transitive_Closure.Get then
            Store_Main_Unit (Arg);
         else
            Store_Sources_To_Process (Arg);
            if not Args_From_Project then
               Argument_File_Specified := True;
            end if;
         end if;
      end loop;
   end Scan_Arguments;

   ---------------------------------
   -- Process_Legacy_Rule_Options --
   ---------------------------------

   procedure Process_Legacy_Rule_Options
     (Args : Arg.Legacy_Rules_Section.Result_Array)
   is
      Remaining_Options : XString_Vector;
   begin
      if Legacy_Rule_Options.Parser.Parse
           (XString_Array (Args), Remaining_Options)
      then
         --  Add coding standard file(s)
         if Legacy_Rule_Options.From_Files.Get'Length = 1 then
            Legacy_Rule_File_Name :=
              Legacy_Rule_Options.From_Files.Get
                (Legacy_Rule_Options.From_Files.Get'First);
         end if;

         for From_File of Legacy_Rule_Options.From_Files.Get loop
            Rule_Options.Append (Option_Record'(File, From_File));
         end loop;

         --  Store legacy rule options
         for Rule_Option of Remaining_Options loop
            Add_Legacy_Rule_Option (To_String (Rule_Option));
            Individual_Rules_Set := True;
         end loop;
      else
         raise Fatal_Error with "cannot parse legacy rule options";
      end if;
   end Process_Legacy_Rule_Options;

end Lkql_Checker.Options;
