with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Directories;   use Ada.Directories;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNAT.Command_Line; use GNAT.Command_Line;

with Lkql_Checker.Output;           use Lkql_Checker.Output;
with Lkql_Checker.Rules.Rule_Table; use Lkql_Checker.Rules.Rule_Table;
with Lkql_Checker.Source_Table;     use Lkql_Checker.Source_Table;

with System.Multiprocessors;

package body Lkql_Checker.Options is

   ------------------------------
   -- Opt_Parse error handling --
   ------------------------------

   procedure Warning (Self : in out Lkql_Checker_Error_Handler; Msg : String)
   is
   begin
      Warning (Msg);
   end Warning;

   procedure Error (Self : in out Lkql_Checker_Error_Handler; Msg : String) is
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

   procedure Scan_Tool_Arguments
     (Args : XString_Array; From_Project_File : Boolean)
   is
      Unknown_Opt_Parse_Args : XString_Vector;
      Exp_It                 : Expansion_Iterator;
      Explicit_Sources       : String_Vector;

      Executable : GNAT.OS_Lib.String_Access :=
        Locate_Exec_On_Path (Command_Name);
      Prefix     : constant String :=
        Containing_Directory (Containing_Directory (Executable.all));
      Lkql       : constant String :=
        Compose (Compose (Prefix, "share"), "lkql");

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
      if From_Project_File then
         declare
            In_Project_Msg : constant String :=
              " is forbidden in project file";
         begin
            Disallow
              (Tool_Args.Transitive_Closure.This, "-U" & In_Project_Msg);
            Disallow
              (Tool_Args.No_Subprojects.This,
               "--no-subprojects" & In_Project_Msg);
            Disallow
              (Tool_Args.Lkql_Path.This, "--lkql-path" & In_Project_Msg);
            Disallow (Tool_Args.Rules.This, "-r" & In_Project_Msg);
            Disallow
              (Tool_Args.Rule_File.This, "--rule-file" & In_Project_Msg);
            Disallow
              (Tool_Args.List_Rules.This, "--list-rules" & In_Project_Msg);
         end;
      end if;

      if not Tool_Args.Parser.Parse
               (Args,
                Unknown_Arguments        => Unknown_Opt_Parse_Args,
                Fallback_On_Command_Line => False)
      then
         raise Parameter_Error;
      end if;

      --  Reallow arguments that were disallowed
      if From_Project_File then
         Allow (Tool_Args.Transitive_Closure.This);
         Allow (Tool_Args.No_Subprojects.This);
         Allow (Tool_Args.Lkql_Path.This);
         Allow (Tool_Args.Rules.This);
         Allow (Tool_Args.Rule_File.This);
         Allow (Tool_Args.List_Rules.This);
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

         else
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
         if Options.Tool_Args.Transitive_Closure.Get then
            Store_Main_Unit (Arg);
         else
            Store_Sources_To_Process (Arg);
            if not From_Project_File then
               Argument_File_Specified := True;
            end if;
         end if;
      end loop;
   end Scan_Tool_Arguments;

   --------------------------
   -- Process_Rule_Options --
   --------------------------

   procedure Process_Rule_Options is
   begin
      --  First of all, process the provided LKQL rule file
      if LKQL_Rule_File_Name /= Null_Unbounded_String then
         Process_LKQL_Rule_File (To_String (LKQL_Rule_File_Name));
      end if;

      --  Then process the legacy rule options
      for O of Rule_Options loop
         case O.Kind is
            when File             =>
               Process_Legacy_Rule_File (To_String (O.Value));

            when Legacy_Option    =>
               Process_Legacy_Rule_Option
                 (To_String (O.Value), Defined_At => "");

            when Single_Rule_Name =>
               Process_Single_Rule_Name (To_String (O.Value));
         end case;
      end loop;
      Process_Compiler_Instances;
   end Process_Rule_Options;

   ---------------------------------
   -- Process_Legacy_Rule_Options --
   ---------------------------------

   procedure Process_Legacy_Rule_Options
     (Args : Tool_Args.Legacy_Rules_Section.Result_Array)
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

   ----------------------------
   -- Add_Legacy_Rule_Option --
   ----------------------------

   procedure Add_Legacy_Rule_Option (Opt : String; Prepend : Boolean := False)
   is
      Opt_Rec : constant Option_Record :=
        (Legacy_Option, To_Unbounded_String (Trim (Opt, Both)));
   begin
      if Prepend then
         Rule_Options.Prepend (Opt_Rec);
      else
         Rule_Options.Append (Opt_Rec);
      end if;
   end Add_Legacy_Rule_Option;

   ----------------------
   -- Add_Rule_By_Name --
   ----------------------

   procedure Add_Rule_By_Name (Rule_Name : String; Prepend : Boolean := False)
   is
      Opt_Rec : constant Option_Record :=
        (Single_Rule_Name, To_Unbounded_String (Trim (Rule_Name, Both)));
   begin
      if Prepend then
         Rule_Options.Prepend (Opt_Rec);
      else
         Rule_Options.Append (Opt_Rec);
      end if;
   end Add_Rule_By_Name;

   ------------------------
   -- Set_LKQL_Rule_File --
   ------------------------

   procedure Set_LKQL_Rule_File (File : String; Project_Relative : Boolean) is
      Resolved_File : constant String :=
        (if Is_Absolute_Path (File)
         then File
         else
           (if Project_Relative
            then Checker_Prj.Get_Project_Relative_File (File)
            else Normalize_Pathname (File)));
   begin
      if LKQL_Rule_File_Name = Null_Unbounded_String then
         LKQL_Rule_File_Name := To_Unbounded_String (Resolved_File);
      else
         Error ("only one LKQL configuration file is allowed");
         Rule_Option_Problem_Detected := True;
      end if;
   end Set_LKQL_Rule_File;

   ---------------------------
   -- Is_Rule_Options_Empty --
   ---------------------------

   function Is_Rule_Options_Empty return Boolean is
   begin
      return
        Rule_Options.Is_Empty
        and then LKQL_Rule_File_Name = Null_Unbounded_String;
   end Is_Rule_Options_Empty;

end Lkql_Checker.Options;
