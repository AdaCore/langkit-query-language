------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2013-2022, AdaCore                     --
--                                                                          --
-- GNATCHECK  is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;            use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with System.Multiprocessors;

with GNATCOLL.Projects.Aux;
with GNATCOLL.Traces;

with GNAT.Strings;
with GNAT.Regexp;       use GNAT.Regexp;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Table;

with Gnatcheck.Compiler;           use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses;
with Gnatcheck.Projects.Aggregate; use Gnatcheck.Projects.Aggregate;
with Gnatcheck.Rules;              use Gnatcheck.Rules;
with Gnatcheck.String_Utilities;   use Gnatcheck.String_Utilities;

with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Source_Table;     use Gnatcheck.Source_Table;

with Rule_Commands; use Rule_Commands;

with Libadalang.Auto_Provider; use Libadalang.Auto_Provider;

package body Gnatcheck.Projects is

   Project_Env      : Project_Environment_Access;
   Project_File_Set : Boolean := False;

   ------------------------------
   -- External variables table --
   ------------------------------

   type X_Var_Record is record
      Var_Name  : String_Access;
      Var_Value : String_Access;
   end record;

   function "<" (Left, Right : X_Var_Record) return Boolean is
     (To_Lower (Left.Var_Name.all) < To_Lower (Right.Var_Name.all));

   function "=" (Left, Right : X_Var_Record)  return Boolean is
     (To_Lower (Left.Var_Name.all) = To_Lower (Right.Var_Name.all));

   package X_Vars_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => X_Var_Record);

   X_Vars : X_Vars_Sets.Set;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Store_Compiler_Option (Switch : String);
   --  Stores compiler option as is

   procedure Load_Aggregated_Project
     (My_Project : in out Arg_Project_Type'Class)
   with Pre => Aggregated_Project;
   --  Loads My_Project (that is supposed to be an aggregate project), then
   --  unloads it and loads in the same environment the project passes as a
   --  parameter of '-A option' (which is supposed to be a (non-aggregate)
   --  project aggregated by My_Project

   function Is_Externally_Built
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean;
   --  Checks if the given source file belongs to an externally build library.

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up (My_Project : Arg_Project_Type) is
      Root_Prj : Project_Type;
      Success  : Boolean;
      Gprbuild : constant String := Global_Report_Dir.all & "gprbuild.err";

   begin
      if Is_Specified (My_Project) then
         Root_Prj := Root_Project (My_Project);

         if Root_Prj /= No_Project then
            GNATCOLL.Projects.Aux.Delete_All_Temp_Files (Root_Prj);
         end if;
      end if;

      if not (Subprocess_Mode or Debug_Mode) then
         Delete_File (Gnatcheck_Config_File.all, Success);
         Delete_File (Gprbuild, Success);
         Delete_File (Gprbuild & ".out", Success);
      end if;
   end Clean_Up;

   --------------------------
   -- Extract_Tool_Options --
   --------------------------

   procedure Extract_Tool_Options (My_Project : in out Arg_Project_Type) is
      Proj : constant Project_Type := Root_Project (My_Project);

      Attr_Def_Switches : constant Attribute_Pkg_List :=
        Build ("Check", "Default_Switches");
      Proj_Args_Parser  : Opt_Parser;

   begin
      if Has_Attribute (Proj, Attr_Def_Switches, "ada") then
         Initialize_Option_Scan
           (Parser                   => Proj_Args_Parser,
            Command_Line             =>
              Attribute_Value (Root_Project (My_Project),
                               Attr_Def_Switches, "ada"),
            Switch_Char              => '-',
            Stop_At_First_Non_Switch => False,
            Section_Delimiters       => "cargs rules");
         Scan_Arguments
           (My_Project  => My_Project,
            Parser      => Proj_Args_Parser,
            In_Switches => False);
      end if;
   end Extract_Tool_Options;

   ------------------------------
   -- Get_Sources_From_Project --
   ------------------------------

   procedure Get_Sources_From_Project (My_Project : in out Arg_Project_Type) is
      File_Patterns_Attribute : constant Attribute_Pkg_List :=
        Build ("codepeer", "file_patterns");

      Patterns : GNAT.Strings.String_List_Access;
      Pattern  : Unbounded_String;
      Prj      : Project_Type;
      Files    : File_Array_Access;

      use Ada.Strings.Unbounded;

   begin
      if Simple_Project then
         --  Use project attribute CodePeer'File_Patterns if set, otherwise
         --  use a default pattern.

         if My_Project.Root_Project.Has_Attribute (File_Patterns_Attribute)
         then
            Patterns := My_Project.Root_Project.Attribute_Value
              (File_Patterns_Attribute);

            if Patterns'Length = 1 then
               Set_Unbounded_String (Pattern, Patterns (1).all);
            else
               Set_Unbounded_String (Pattern, "{");

               for J in Patterns'First .. Patterns'Last - 1 loop
                  Append (Pattern, Patterns (J).all & ",");
               end loop;

               Append (Pattern, Patterns (Patterns'Last).all & "}");
            end if;
         else
            Set_Unbounded_String (Pattern, "*.{ad[asb],spc,bdy}");
         end if;

         Files := Find_Files_Regexp
           (Name_Pattern =>
              Compile (To_String (Pattern),
                       Glob           => True,
                       Case_Sensitive => False),
            Directories =>
              Source_Dirs
                (My_Project.Root_Project,
                 Recursive => U_Option_Set,
                 Include_Externally_Built => False));

         for F of Files.all loop
            Store_Sources_To_Process (F.Display_Full_Name);
         end loop;

         My_Project.Files := Files;

      elsif No_Argument_File_Specified
        or else (U_Option_Set and then (not File_List_Specified))
      then
         if Main_Unit = null
         --  ??? Call Files_From_Closure once available in libgpr2 instead
         --  when Main_Unit is set
           or else not File_List_Specified
         then
            Prj := My_Project.Root_Project;

            Files := Prj.Source_Files (Recursive => U_Option_Set);

            for F in Files'Range loop
               if not Is_Externally_Built (Files (F), My_Project)
                 and then Is_Ada_File (Files (F), My_Project)
               then
                  Store_Sources_To_Process (Files (F).Display_Base_Name);
               end if;
            end loop;

            if U_Option_Set then
               if Files'Length = 0 then
                  Unchecked_Free (Files);
                  Error (My_Project.Source_Prj.all &
                         " does not contain source files");
                  return;
               end if;
            else
               Prj := Extended_Project (Prj);

               while Prj /= No_Project loop
                  Unchecked_Free (Files);
                  Files := Prj.Source_Files (Recursive => False);

                  for F in Files'Range loop
                     if not Is_Externally_Built (Files (F), My_Project)
                       and then
                        Is_Ada_File (Files (F), My_Project)
                     then
                        Gnatcheck.Source_Table.Store_Sources_To_Process
                          (Files (F).Display_Base_Name);
                     end if;
                  end loop;

                  Prj := Extended_Project (Prj);
               end loop;
            end if;

            Unchecked_Free (Files);
         end if;
      end if;
   end Get_Sources_From_Project;

   ----------------------------
   -- Initialize_Environment --
   ----------------------------

   procedure Initialize_Environment is
   begin
      GNATCOLL.Traces.Parse_Config_File;
      Initialize (Project_Env);

      Project_Env.Set_Target_And_Runtime (Target.all, RTS_Path.all);

      if Follow_Symbolic_Links then
         Project_Env.Set_Trusted_Mode (True);
      end if;
   end Initialize_Environment;

   -----------------
   -- Is_Ada_File --
   -----------------

   function Is_Ada_File
     (File       : Virtual_File;
      My_Project : Arg_Project_Type) return Boolean is
   begin
      return To_Lower (Language (Info (My_Project, File))) = "ada";
   end Is_Ada_File;

   -------------------------
   -- Is_Externally_Built --
   -------------------------

   function Is_Externally_Built
     (File :       Virtual_File;
      My_Project : Arg_Project_Type)
      return Boolean
   is
      F_Info : constant File_Info    := Info (My_Project, File);
      Proj   : constant Project_Type := Project (F_Info);
      Attr   : constant Attribute_Pkg_String := Build ("", "externally_built");
   begin
      if Has_Attribute (Proj, Attr) then
         if Attribute_Value (Proj, Attr) = "true" then
            return True;
         end if;
      end if;
      return False;
   end Is_Externally_Built;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean is
   begin
      return My_Project.Source_Prj /= null;
   end Is_Specified;

   -----------
   -- Files --
   -----------

   function Files (My_Project : Arg_Project_Type) return File_Array_Access is
   begin
      return My_Project.Files;
   end Files;

   -----------------------------
   -- Load_Aggregated_Project --
   -----------------------------

   procedure Load_Aggregated_Project
     (My_Project : in out Arg_Project_Type'Class)
   is
      procedure Errors (S : String);
      --  Callback used by the call to Load below to emit a project loading
      --  error.

      procedure Errors (S : String) is
      begin
         if Index (S, " not a regular file") /= 0 then
            Error ("project file " & My_Project.Source_Prj.all & " not found");
         elsif Index (S, "is illegal for typed string") /= 0 then
            Error (S);
            raise Parameter_Error;
         elsif Index (S, "warning") /= 0
              and then Index (S, "directory") /= 0
              and then Index (S, "not found") /= 0
         then
            return;
         else
            Error (S);
         end if;
      end Errors;
   begin
      My_Project.Load
        (GNATCOLL.VFS.Create (+My_Project.Source_Prj.all),
         Project_Env,
         Errors              => Errors'Unrestricted_Access,
         Report_Missing_Dirs => False);

      if My_Project.Root_Project = No_Project then
         Error ("project not loaded");
      end if;

      pragma Assert (Is_Aggregate_Project (My_Project.Root_Project));

      My_Project.Unload;

      if Subdir_Name /= null then
         Set_Object_Subdir (Project_Env.all, +Subdir_Name.all);
      end if;

      Load
        (Self                => My_Project,
         Root_Project_Path   => Create (Filesystem_String
                                          (Get_Aggregated_Project)),
         Env                 => Project_Env,
         Errors              => Errors'Unrestricted_Access,
         Report_Missing_Dirs => False);
   end Load_Aggregated_Project;

   -----------------------
   -- Load_Tool_Project --
   -----------------------

   procedure Load_Tool_Project (My_Project : in out Arg_Project_Type) is
      Aggregated_Prj_Name : Filesystem_String_Access;

      procedure Errors (S : String);
      procedure Errors (S : String) is
      begin
         if Index (S, " not a regular file") /= 0 then
            Error ("project file " & My_Project.Source_Prj.all & " not found");
         elsif Index (S, "is illegal for typed string") /= 0 then
            Error (S);
            raise Parameter_Error;
         elsif Index (S, "warning") /= 0
              and then Index (S, "directory") /= 0
              and then Index (S, "not found") /= 0
         then
            return;
         else
            Error (S);
         end if;
      end Errors;

      procedure Ignore (S : String) is null;
      --  Ignore result of function returning a String

   begin
      if Subdir_Name /= null then
         Set_Object_Subdir (Project_Env.all, +Subdir_Name.all);
      end if;

      Ignore
        (Register_New_Attribute
          ("file_patterns", "codepeer", Is_List => True));

      My_Project.Load
        (GNATCOLL.VFS.Create (+My_Project.Source_Prj.all),
         Project_Env,
         Errors              => Errors'Unrestricted_Access,
         Report_Missing_Dirs => False);

      if Is_Aggregate_Project (My_Project.Root_Project) then
         if My_Project.Root_Project = No_Project then
            Error ("project not loaded");
         end if;

         Collect_Aggregated_Projects (My_Project.Root_Project);
         N_Of_Aggregated_Projects := Num_Of_Aggregated_Projects;

         case N_Of_Aggregated_Projects is
            when 0 =>
               --  Pathological case, but we need to generate a reasonable
               --  message
               Error
                 ("aggregate project does not contain anything to process");
               raise Parameter_Error;

            when 1 =>
               --  Important and useful particular case - exactly one project
               --  is aggregated, so we load it in the environment that already
               --  has all the settings from the argument aggregate project:

               Aggregated_Prj_Name := new Filesystem_String'
                 (Full_Name (Get_Aggregated_Prj_Src));

               My_Project.Unload;

               Load
                 (Self                => My_Project,
                  Root_Project_Path   => Create (Aggregated_Prj_Name.all),
                  Env                 => Project_Env,
                  Errors              => Errors'Unrestricted_Access,
                  Report_Missing_Dirs => False);
               Free (Aggregated_Prj_Name);

            when others =>
               --  General case - more than one project is aggregated. We have
               --  process them one by one spawning gnatcheck for each project.

               null;
         end case;
      end if;

   exception
      when Invalid_Project =>
         raise Parameter_Error;
   end Load_Tool_Project;

   --------------------------
   -- Process_Project_File --
   --------------------------

   procedure Process_Project_File
     (My_Project : in out Arg_Project_Type'Class) is
   begin
      if not My_Project.Is_Specified then
         return;
      end if;

      Register_Tool_Attributes (My_Project);
      Initialize_Environment;
      Set_External_Values (My_Project);

      if Aggregated_Project then
         Load_Aggregated_Project (My_Project);
      else
         Load_Tool_Project (My_Project);
      end if;

      if N_Of_Aggregated_Projects > 1 then
         if not No_Argument_File_Specified then
            Error ("no argument file should be specified if aggregate " &
                   "project");
            Error_No_Tool_Name
              ("aggregates more than one non-aggregate project");

            raise Parameter_Error;
         end if;

         if Main_Unit /= null then
            Error ("'-U main' cannot be used if aggregate project");
            Error_No_Tool_Name
              ("aggregates more than one non-aggregate project");

            raise Parameter_Error;
         end if;

         --  No information is extracted from the aggregate project
         --  itself
         In_Aggregate_Project := True;
         return;
      else
         Get_Sources_From_Project (My_Project);
      end if;
   end Process_Project_File;

   -------------------------------
   -- Report_Aggregated_Project --
   -------------------------------

   procedure Report_Aggregated_Project
     (Aggregate_Prj          : Arg_Project_Type;
      Aggregated_Prj_Name    : String;
      Expected_Text_Out_File : String;
      Expected_XML_Out_File  : String)
   is
      pragma Unreferenced (Aggregate_Prj);
   begin

      if Text_Report_ON then
         Report ("");
         Report ("Processing aggregated project " & Aggregated_Prj_Name);
         Report ("Expected report file: " & Expected_Text_Out_File);
      end if;

      if XML_Report_ON then
         XML_Report ("<aggregated-project>",
                     Indent_Level => 2);

         XML_Report ("<project-file>" & Aggregated_Prj_Name &
                     "</project-file>",
                     Indent_Level => 3);

         XML_Report ("<report-file>" & Expected_XML_Out_File &
                     "</report-file>",
                     Indent_Level => 3);
      end if;
   end Report_Aggregated_Project;

   -------------------------
   -- Set_External_Values --
   -------------------------

   procedure Set_External_Values (My_Project : Arg_Project_Type) is
      GPR_TOOL_Set : Boolean := False;
   begin
      for Var of X_Vars loop
         Project_Env.Change_Environment
           (Var.Var_Name.all, Var.Var_Value.all);

         if Var.Var_Name.all = "GPR_TOOL" then
            GPR_TOOL_Set := True;
         end if;
      end loop;

      --  Set GPR_TOOL, if needed

      if not Ada.Environment_Variables.Exists ("GPR_TOOL")
        and then not GPR_TOOL_Set
      then
         Project_Env.Change_Environment ("GPR_TOOL", "gnatcheck");
      end if;
   end Set_External_Values;

   ----------------------------
   -- Set_Global_Result_Dirs --
   ----------------------------

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type) is
      Dir : Virtual_File;
   begin
      if not No_Object_Dir and then Gnatcheck_Prj.Is_Specified then
         Dir := My_Project.Root_Project.Object_Dir;

         if Dir = No_File then
            Dir := My_Project.Root_Project.Project_Path;
         end if;
      else
         Dir := Get_Current_Dir;
      end if;

      GNAT.OS_Lib.Free (Global_Report_Dir);
      Global_Report_Dir := new String'(Display_Dir_Name (Dir));
   end Set_Global_Result_Dirs;

   ---------------------
   -- Set_Subdir_Name --
   ---------------------

   procedure Set_Subdir_Name (S : String) is
   begin
      Free (Subdir_Name);
      Subdir_Name := new String'(S);
   end Set_Subdir_Name;

   ----------------
   -- Source_Prj --
   ----------------

   function Source_Prj (My_Project : Arg_Project_Type) return String is
   begin
      if Is_Specified (My_Project) then
         return My_Project.Source_Prj.all;
      else
         return "";
      end if;
   end Source_Prj;

   -----------------------------
   -- Store_External_Variable --
   -----------------------------

   procedure Append_Variables
     (Args : in out Argument_List;
      Last : in out Natural) is
   begin
      for Var of X_Vars loop
         Last := Last + 1;
         Args (Last) :=
           new String'("-X" & Var.Var_Name.all & "=" & Var.Var_Value.all);
      end loop;
   end Append_Variables;

   -----------------------------
   -- Store_External_Variable --
   -----------------------------

   procedure Store_External_Variable (Var : String) is
      Var_Name_Start  : constant Natural := Var'First;
      Var_Name_End    :          Natural := Index (Var, "=");
      Var_Value_Start :          Natural;
      Var_Value_End   : constant Natural := Var'Last;

      New_Var_Rec : X_Var_Record;

      use X_Vars_Sets;
      C : Cursor;
   begin
      if Var_Name_End <= Var_Name_Start then
         Error ("wrong parameter of -X option: " & Var);
         raise Parameter_Error;
      else
         Var_Name_End    := Var_Name_End - 1;
         Var_Value_Start := Var_Name_End + 2;
         New_Var_Rec    :=
           (Var_Name  => new String'(Var (Var_Name_Start .. Var_Name_End)),
            Var_Value => new String'(Var (Var_Value_Start .. Var_Value_End)));
      end if;

      C := Find (X_Vars, New_Var_Rec);

      if Has_Element (C) then
         Replace_Element (Container => X_Vars,
                          Position  => C,
                          New_Item  => New_Var_Rec);

      else
         Insert (X_Vars, New_Var_Rec);
      end if;
   end Store_External_Variable;

   ---------------------
   -- Store_Main_Unit --
   ---------------------

   procedure Store_Main_Unit
     (Unit_Name   : String;
      Store       : Boolean := True) is
   begin
      if Store then
         if Gnatcheck.Projects.Main_Unit = null then
            Gnatcheck.Projects.Main_Unit := new String'(Unit_Name);
         else
            Error ("cannot specify more than one main after -U");
            raise Parameter_Error;
         end if;
      end if;
   end Store_Main_Unit;

   --------------------------
   -- Store_Project_Source --
   --------------------------

   procedure Store_Project_Source
     (My_Project         : in out Arg_Project_Type;
      Project_File_Name  : String)
   is
      Ext : constant String :=
        (if Has_Suffix (Project_File_Name, Suffix => ".gpr")
         then "" else ".gpr");

   begin
      if Project_File_Set then
         Error ("cannot have several project files specified");
         raise Parameter_Error;
      else
         Project_File_Set := True;
      end if;

      My_Project.Source_Prj := new String'(Project_File_Name & Ext);
   end Store_Project_Source;

   -------------------------------------
   -- Aggregate_Project_Report_Header --
   -------------------------------------

   procedure Aggregate_Project_Report_Header
     (My_Project : Arg_Project_Type) is
   begin
      if XML_Report_ON then
         XML_Report ("<?xml version=""1.0""?>");
         XML_Report_No_EOL ("<gnatcheck-report");

         if Gnatcheck_Prj.Is_Specified then
            XML_Report (" project=""" & Gnatcheck_Prj.Source_Prj.all & """>");
         else
            XML_Report (">");
         end if;
      end if;

      Gnatcheck.Diagnoses.Print_Report_Header;

      if Text_Report_ON then
         Report ("");
         Report ("Argument project is an aggregate project");
         Report ("Aggregated projects are processed separately");
      end if;

      if XML_Report_ON then
         XML_Report ("<aggregated-project-reports>",
                     Indent_Level => 1);
         Report ("");
      end if;
   end Aggregate_Project_Report_Header;

   ------------------------------------
   -- Close_Aggregate_Project_Report --
   ------------------------------------

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type) is
   begin
      if XML_Report_ON then
         XML_Report ("</aggregated-project-reports>",
                     Indent_Level => 1);
         XML_Report ("</gnatcheck-report>");
      end if;
   end Close_Aggregate_Project_Report;

   -----------------------------------------
   -- Report_Aggregated_Project_Exit_Code --
   -----------------------------------------

   procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Arg_Project_Type;
      Exit_Code     : Integer)
   is
      pragma Unreferenced (Aggregate_Prj);
   begin
      if Text_Report_ON then
         Report ("Exit code is" & Exit_Code'Img & " (" &
                 (case Exit_Code is
                     when 0 => "no rule violation detected",
                     when 1 => "rule violation(s) detected",
                     when 2 => "tool failure, results cannot be trusted",
                     when 3 => "no rule check performed",
                     when others => "unknown")        & ")");
      end if;

      if XML_Report_ON then
         XML_Report ("<exit-code>" & Image (Exit_Code) & "</exit-code>",
                     Indent_Level => 3);

         XML_Report ("</aggregated-project>",
                     Indent_Level => 2);
      end if;
   end Report_Aggregated_Project_Exit_Code;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (My_Project  : in out Arg_Project_Type;
      First_Pass  :         Boolean    := False;
      Parser      :         Opt_Parser := Command_Line_Parser;
      In_Switches :         Boolean    := False)
   is
      procedure Process_Sections;
      --  Processes the 'rules' section.

      procedure Process_Sections is
      begin
         --  Processing the 'cargs' section

         Goto_Section ("cargs", Parser => Parser);

         while GNAT.Command_Line.Getopt ("*", Parser => Parser) /= ASCII.NUL
         loop
            Store_Compiler_Option (Full_Switch (Parser => Parser));
         end loop;

         --  Processing the 'rules' section
         Goto_Section ("rules", Parser => Parser);

         loop
            case GNAT.Command_Line.Getopt ("* from=", Parser => Parser) is
            --  We do not want to depend on the set of the currently
            --  implemented rules
               when ASCII.NUL =>
                  exit;
               when 'f' =>
                  Process_Rule_File (Parameter (Parser => Parser));

                  if not More_Then_One_Rule_File_Set then
                     Rule_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     More_Then_One_Rule_File_Set := True;
                  else
                     Free (Rule_File_Name);
                  end if;

               when others =>
                  Process_Rule_Option
                    (Ada.Strings.Fixed.Trim
                       (Full_Switch (Parser => Parser), Ada.Strings.Both),
                    Defined_At => "");
                  --  We use the call to Trim here because there can be a rule
                  --  option in quotation marks
                  Individual_Rules_Set := True;
            end case;
         end loop;
      end Process_Sections;

      Executable : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path
        (Ada.Command_Line.Command_Name);
      Prefix     : constant String :=
         Containing_Directory (Containing_Directory (Executable.all));
      Lkql       : constant String :=
        Compose (Compose (Prefix, "share"), "lkql");

      In_Project_File : constant Boolean := Parser /= Command_Line_Parser;
      Initial_Char    : Character;
      Success         : Boolean;

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

      loop
         Initial_Char :=
           GNAT.Command_Line.Getopt
             ("v q t h hx s "             &
              "m? files= a "              &
              "P: U X! vP! eL A: "        &   --  project-specific options
              "-brief "                   &
              "-charset= "                &
              "-check-semantic "          &
              "-check-redefinition "      &
              "-no_objects_dir "          &
              "-subdirs= "                &
              "-target= "                 &
              "-kp-version= "             &
              "j! "                       &
              "d dd dkp "                 &
              "o= "                       &
              "ox= "                      &
              "-RTS= "                    &
              "l log "                    &
              "-include-file= "           &
              "-rules-dir= "              &
              "-show-rule "               &
              "-subprocess "              &
              "-version -help "           &
              "-ignore= "                 &
              "-ignore-project-switches " &
              "-simple-project "          &
              "nt xml",
              Parser => Parser);

         case Initial_Char is
            when ASCII.NUL =>
               Success := False;

               loop
                  declare
                     Arg    : constant String := Get_Argument
                       (Do_Expansion => True,
                        Parser       => Command_Line_Parser);

                  begin
                     exit when Arg = "";
                     Success := True;

                     if In_Switches then
                        Error ("Switches attribute cannot contain argument " &
                               "sources");
                        raise Parameter_Error;
                     end if;

                     if Gnatcheck.Projects.U_Option_Set then
                        Gnatcheck.Projects.Store_Main_Unit
                          (Arg, In_Project_File or First_Pass);
                     else
                        Store_Sources_To_Process
                          (Arg, In_Project_File or First_Pass);
                     end if;
                  end;
               end loop;

               exit when not Success;

            when 'A' =>
               if Full_Switch (Parser => Parser) = "A" then
                  if First_Pass then
                     Aggregated_Project := True;
                     Gnatcheck.Projects.Aggregate.Store_Aggregated_Project
                       (Parameter);
                  elsif In_Project_File then
                     Error ("project file should not be specified inside " &
                            "another project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'a' =>
               --  Ignore -a for compatibility

               null;

            when 'd' =>
               if First_Pass then
                  if Full_Switch (Parser => Parser) = "dkp" then
                     Gnatkp_Mode := True;
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "d" then
                     Debug_Mode := True;
                  elsif Full_Switch (Parser => Parser) = "dd" then
                     Progress_Indicator_Mode := True;
                  end if;
               end if;

            when 'e' =>
               if Full_Switch (Parser => Parser) = "eL" then
                  if First_Pass then
                     Gnatcheck.Projects.Follow_Symbolic_Links := True;
                  elsif In_Project_File then
                     Error ("-eL option cannot be set in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'f' =>
               if Full_Switch (Parser => Parser) = "files" then
                  File_List_Specified := True;

                  if First_Pass then
                     Files_Switch_Used := True;
                     Read_Args_From_File (Parameter (Parser => Parser));

                  elsif In_Project_File then
                     if In_Switches then
                        Error ("-files option is not allowed " &
                               "for Switches attribute");
                        raise Parameter_Error;
                     else
                        Read_Args_From_File (Parameter (Parser => Parser));
                     end if;
                  end if;
               end if;

            when 'h' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "h" then
                     Generate_Rules_Help := True;
                  elsif Full_Switch (Parser => Parser) = "hx" then
                     Generate_XML_Help := True;
                  end if;
               end if;

            when 'j' =>
               if Full_Switch (Parser => Parser) = "j"
                 and then not First_Pass
               then
                  begin
                     J_Specified := True;
                     Process_Num :=
                       Natural'Value (Parameter (Parser => Parser));

                     if Process_Num = 0 then
                        Process_Num :=
                          Positive (System.Multiprocessors.Number_Of_CPUs);
                     end if;
                  exception
                     when Constraint_Error =>
                        Error ("Wrong Parameter of '-j' option: " &
                               Parameter (Parser => Parser));
                        raise Parameter_Error;
                  end;
               end if;

            when 'l' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "l" then
                     Full_Source_Locations := True;
                  elsif Full_Switch (Parser => Parser) = "log" then
                     Log_Mode := True;
                  end if;
               end if;

            when 'm' =>
               if not First_Pass then
                  begin
                     Max_Diagnoses :=
                       Natural'Value (Parameter (Parser => Parser));

                     if Max_Diagnoses > 1000 then
                        Error ("Parameter (Parser => Parser) of '-m' option " &
                               "too big, max allowed is 1000");
                        raise Parameter_Error;
                     end if;

                  exception
                     when Constraint_Error =>
                     Error ("Wrong Parameter of '-m' option: " &
                            Parameter (Parser => Parser));
                     raise Parameter_Error;
                  end;
               end if;

            when 'n' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "nt" then
                     Text_Report_ON := False;
                     XML_Report_ON  := True;
                  end if;
               end if;

            when 'o' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     Set_Report_File_Name (Parameter (Parser => Parser));
                     Custom_Text_Report_File := True;

                  elsif Full_Switch (Parser => Parser) = "ox" then
                     Set_XML_Report_File_Name (Parameter (Parser => Parser));
                     XML_Report_ON          := True;
                     Custom_XML_Report_File := True;
                  end if;
               end if;

            when 'P' =>
               if Full_Switch (Parser => Parser) = "P" then
                  if First_Pass then
                     My_Project.Store_Project_Source (Parameter);
                  elsif In_Project_File then
                     Error ("project file should not be specified inside " &
                            "another project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'q' =>
               if not First_Pass then
                  Quiet_Mode := True;
               end if;

            when 's' =>
               if not First_Pass then
                  Short_Report := True;
               end if;

            when 't' =>
               if not First_Pass then
                  Compute_Timing := True;
               end if;

            when 'U' =>
               if Full_Switch (Parser => Parser) = "U" then
                  if First_Pass then
                     if Gnatcheck.Projects.U_Option_Set then
                        Error ("-U can be specified only once");
                        raise Parameter_Error;
                     end if;

                     Gnatcheck.Projects.U_Option_Set := True;
                  elsif In_Project_File then
                     Error ("-U option is not allowed in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'v' =>
               if Full_Switch (Parser => Parser) = "v" then
                  Verbose_Mode := True;
               elsif Full_Switch (Parser => Parser) = "vP" then
                  if First_Pass then
                     begin
                        Gnatcheck.Projects.Verbosity_Level :=
                          Verbosity_Levels'Value (Parameter);
                     exception
                        when Constraint_Error =>
                           Error ("wrong switch parameter " &
                                  Parameter & " for -vP");
                           raise Parameter_Error;
                     end;
                  elsif In_Project_File then
                     Error ("-vP option is not allowed in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'x' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "xml" then
                     XML_Report_ON  := True;
                  end if;
               end if;

            when 'X' =>
               if Full_Switch (Parser => Parser) = "X" then
                  if First_Pass then
                     Gnatcheck.Projects.Store_External_Variable
                       (Var => Parameter);
                  elsif In_Project_File then
                     Error ("external references cannot be set in " &
                            "a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when '-' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "-brief" then
                     Quiet_Mode   := True;
                     Short_Report := True;
                     Brief_Mode   := True;

                  elsif Full_Switch (Parser => Parser) = "-charset" then
                     Free (Charset);
                     Charset := new String'(Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "-check-redefinition"
                  then
                     Check_Param_Redefinition := True;
                  elsif Full_Switch (Parser => Parser) = "-check-semantic" then
                     Check_Semantic := True;

                  elsif Full_Switch (Parser => Parser) = "-ignore" then
                     if Is_Regular_File (Parameter (Parser => Parser)) then
                        Exempted_Units :=
                          new String'(Normalize_Pathname
                                        (Parameter (Parser => Parser)));
                     else
                        Error (Parameter (Parser => Parser) & " not found");
                        raise Parameter_Error;
                     end if;

                  elsif Full_Switch (Parser => Parser) = "-include-file" then
                     Gnatcheck.Diagnoses.Process_User_Filename
                       (Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "-kp-version" then
                     Free (KP_Version);
                     KP_Version := new String'(Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "-simple-project" then
                     Simple_Project := True;

                     --  --simple-project is often used in the context of
                     --  CodePeer where "gnatls" may not be available. So if
                     --  Target hasn't been set explicitly and codepeer-gnatls
                     --  is available, force its use by setting the "codepeer"
                     --  target.

                     if Target'Length = 0
                       and then Locate_Exec_On_Path ("codepeer-gnatls") /= null
                     then
                        Free (Target);
                        Target := new String'("codepeer");
                     end if;

                  elsif Full_Switch (Parser => Parser) = "-show-rule" then
                     Mapping_Mode := True;

                  elsif Full_Switch (Parser => Parser) = "-subprocess" then
                     Subprocess_Mode := True;
                     Quiet_Mode      := True;
                     Brief_Mode      := True;

                  elsif Full_Switch (Parser => Parser) = "-RTS" then
                     --  We do not store --RTS option for gcc now - we have
                     --  to resolve its parameter to the full path, and we
                     --  can do this only when target is fully detected.
                     null;
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-help" then
                     if In_Project_File then
                        Error
                          ("project file should not contain '--help' option");
                        raise Parameter_Error;
                     end if;

                     Print_Usage := True;
                     return;

                  elsif Full_Switch (Parser => Parser) = "-version" then
                     if In_Project_File then
                        Error
                          ("project file should not contain '--version' " &
                           "option");
                        raise Parameter_Error;
                     end if;

                     Print_Version := True;

                  elsif Full_Switch (Parser => Parser) =
                        "-ignore-project-switches"
                  then
                     Ignore_Project_Switches := True;

                  elsif Full_Switch (Parser => Parser) = "-target" then
                     Free (Target);
                     Target := new String'(Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "-RTS" then
                     Free (RTS_Path);
                     RTS_Path := new String'(Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "-subdirs" then
                     Set_Subdir_Name (Parameter (Parser => Parser));

                  elsif Full_Switch (Parser => Parser) = "-no_objects_dir" then
                     No_Object_Dir := True;

                  elsif Full_Switch (Parser => Parser) = "-rules-dir"
                    and then not Legacy
                  then
                     Additional_Rules_Dirs.Append
                       (Parameter (Parser => Parser));
                  end if;
               end if;

            when others =>
               Error
                 ("unrecognized switch: " & Full_Switch (Parser => Parser));
               raise Parameter_Error;
         end case;
      end loop;

      if Current_Section (Parser => Parser) = ""
        and then not First_Pass
      then
         Process_Sections;
      end if;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Error ("invalid switch: " & Full_Switch (Parser => Parser));
         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Error ("missing Parameter (Parser => Parser) for: -" &
                Full_Switch (Parser => Parser));
         raise Parameter_Error;
   end Scan_Arguments;

   ---------------------------
   -- Store_Compiler_Option --
   ---------------------------

   package Compiler_Switches is new GNAT.Table (
      Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Compiler options");

   procedure Store_Compiler_Option (Switch : String) is
   begin
      Compiler_Switches.Append (new String'(Switch));
   end Store_Compiler_Option;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is
   begin
      if Verbose_Mode and then not Aggregated_Project then
         --  When procressing aggregated projects one by one, we want
         --  Verbose_Mode to print this only in the outer invocation.
         Print_Version_Info (2004);
      end if;

      if Print_Version then
         Print_Tool_Version (2004);
         Nothing_To_Do := True;
         return;
      end if;

      if Print_Usage then
         Print_Gnatcheck_Usage;
         Nothing_To_Do := True;
         return;
      end if;

      --  We generate the rule help unconditionally

      if Generate_Rules_Help and then not Aggregated_Project then
         Rules_Help;
      end if;

      if Gnatcheck.Options.Generate_XML_Help
        and then not Aggregated_Project
      then
         XML_Help;
      end if;

      if In_Aggregate_Project then
         --  We have to skip most of the checks because this call does not do
         --  anything except spawning another gnatcheck for individual projects

         Set_Global_Result_Dirs (Gnatcheck_Prj);
         goto Processing_Aggregate_Project;
      end if;

      --  Check the correctness of setting custom name for text report file

      if Custom_Text_Report_File and then not Text_Report_ON then
         Error ("Custom text output file cannot be set if text output is off");
         raise Parameter_Error;
      end if;

      --  No need to perform similar checks for custom XML file because it can
      --  be set only with turning ON XML output

      --  Now check if we have anything to do:

      if No_Argument_File_Specified then
         if Generate_Rules_Help or else Generate_XML_Help then
            Nothing_To_Do := True;
            return;
         else
            Error ("No input source file set");
            raise Parameter_Error;
         end if;
      end if;

      Read_Args_From_Temp_Storage
        (Duplication_Report => not Is_Specified (Gnatcheck_Prj),
         Arg_Project        => Gnatcheck_Prj);
      Nothing_To_Do := Last_Source < First_SF_Id;

      if Nothing_To_Do then
         Error ("No existing file to process");
         return;
      end if;

      Set_Compiler_Checks;

      Gnatcheck.Projects.Set_Global_Result_Dirs (Gnatcheck_Prj);
      Gnatcheck_Config_File :=
        new String'(Global_Report_Dir.all & Gnatcheck_Config_File.all);

      Analyze_Compiler_Output :=
        Use_gnaty_Option or Use_gnatw_Option or
        Check_Restrictions or Check_Semantic;

      if Analyze_Compiler_Output then
         Store_Compiler_Option ("-gnatcU");

         if Use_gnatw_Option then
            Store_Compiler_Option ("-gnatwnA");
            Store_Compiler_Option (Get_Warning_Option);

            if Mapping_Mode then
               Store_Compiler_Option ("-gnatw.d");
            else
               Store_Compiler_Option ("-gnatw.D");
            end if;

            Store_Compiler_Option ("-gnatec=" & Gnatcheck_Config_File.all);

         elsif Check_Restrictions then
            Store_Compiler_Option ("-gnatwnA");
            Store_Compiler_Option ("-gnatec=" & Gnatcheck_Config_File.all);
         else
            --  '-gnatws' disables all the warnings except style-related
            Store_Compiler_Option ("-gnatws");
         end if;

         Store_Compiler_Option ("-gnatyN");

         if Use_gnaty_Option then
            for S of Create (Get_Style_Option, " ") loop
               Store_Compiler_Option (S);
            end loop;
         end if;
      end if;

      if Gnatkp_Mode and then KP_Version /= null then
         for Rule in All_Rules.First .. All_Rules.Last loop
            if All_Rules.Table (Rule).Impact /= null
              and then Match (KP_Version.all,
                              All_Rules.Table (Rule).Impact.all)
            then
               if All_Rules.Table (Rule).Target /= null
                 and then Target.all /= ""
                 and then not Match (Target.all,
                                     All_Rules.Table (Rule).Target.all)
               then
                  if not Quiet_Mode then
                     Info ("info: " & All_Rules.Table (Rule).Name.all &
                           " disabled, target does not match");
                  end if;
               else
                  if not Quiet_Mode then
                     Info ("info: " & All_Rules.Table (Rule).Name.all &
                           " enabled");
                  end if;

                  Set_Rule_State (Rule, Enabled);
               end if;
            end if;
         end loop;
      end if;

      for Rule in All_Rules.First .. All_Rules.Last loop
         if Is_Enabled (All_Rules.Table (Rule).all) then
            Active_Rule_Present := True;
            exit;
         end if;
      end loop;

      if not (Active_Rule_Present or else Analyze_Compiler_Output) then
         Error ("No rule to check specified");
         raise Parameter_Error;
      end if;

      if Exempted_Units /= null then
         Process_Exemptions (Exempted_Units.all);
      end if;

      Total_Sources := Total_Sources_To_Process;

      if Total_Sources = 0 then
         Error ("No existing file to process");
         Nothing_To_Do := True;
         return;
      end if;

      --  If we are here - we have sources to check and rules to apply

      Sources_Left := Total_Sources;

      --  Save compiler switches computed in Compiler_Arg_List

      Compiler_Arg_List := new Argument_List'
        (String_List (Compiler_Switches.Table (1 .. Compiler_Switches.Last)));

      <<Processing_Aggregate_Project>>

      Gnatcheck.Output.Set_Report_Files;
   end Check_Parameters;

end Gnatcheck.Projects;
