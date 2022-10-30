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

with Ada.Calendar;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;            use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with System.Multiprocessors;

with GNATCOLL.Traces;

with GPR2.Containers;
with GPR2.Context;
with GPR2.KB;
with GPR2.Log;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Source.Set;
with GPR2.Project.View;
with GPR2.Project.View.Set;

with GNAT.Directory_Operations;
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

   Project_Context  : GPR2.Context.Object;
   Project_File_Set : Boolean := False;

   Default_Switches_Attr : constant GPR2.Q_Attribute_Id :=
     (GPR2."+"("Check"), GPR2."+"("Default_Switches"));
   File_Patterns_Attr    : constant GPR2.Q_Attribute_Id :=
     (GPR2."+"("CodePeer"), GPR2."+"("File_Patterns"));

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

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up (My_Project : Arg_Project_Type) is
      Success  : Boolean;
      Gprbuild : constant String := Global_Report_Dir.all & "gprbuild.err";

   begin
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
      Proj : constant GPR2.Project.View.Object := My_Project.Tree.Root_Project;

      use GPR2;
      use GPR2.Project.Registry.Attribute;

      Proj_Args_Parser : Opt_Parser;
      Ada_Idx          : constant GPR2.Project.Attribute_Index.Object :=
        GPR2.Project.Attribute_Index.Create (Ada_Language);
      Attr             : GPR2.Project.Attribute.Object;
      Command_Line     : GNAT.OS_Lib.Argument_List_Access;

   begin
      if Proj.Has_Attribute (Default_Switches_Attr, Ada_Idx) then
         Attr := Proj.Attribute (Default_Switches_Attr, Ada_Idx);

         if Attr.Kind = Single then
            Error
              (String (Proj.Path_Name.Simple_Name)
               & ": Check.Default_Switches value must be a list");
            raise Parameter_Error;
         end if;

         Command_Line := new String_List
           (Attr.Values.First_Index .. Attr.Values.Last_Index);
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index loop
            Command_Line (J) := new String'(Attr.Values.Element (J).Text);
         end loop;

         Initialize_Option_Scan
           (Parser                   => Proj_Args_Parser,
            Command_Line             => Command_Line,
            Switch_Char              => '-',
            Stop_At_First_Non_Switch => False,
            Section_Delimiters       => "cargs rules");
         Scan_Arguments
           (My_Project  => My_Project,
            Parser      => Proj_Args_Parser,
            In_Switches => False);

         Free (Command_Line);
      end if;
   end Extract_Tool_Options;

   ------------------------------
   -- Get_Sources_From_Project --
   ------------------------------

   procedure Get_Sources_From_Project (My_Project : in out Arg_Project_Type) is
      use GPR2;
      use GPR2.Project.Registry.Attribute;
      use GPR2.Project.View;
      use GPR2.Project.Source.Set;

      Proj : constant GPR2.Project.View.Object := My_Project.Tree.Root_Project;
      Attr : GPR2.Project.Attribute.Object;

      Patterns    : GNAT.Strings.String_List_Access;
      Pattern     : Unbounded_String;
      Files       : File_Array_Access;
      Directories : File_Array_Access;

      procedure Store_Source (Source : Project.Source.Object);
      --  Callback used to store sources

      procedure Add_Src
        (Source    : GPR2.Project.Source.Object;
         Index     : GPR2.Unit_Index;
         Timestamp : Ada.Calendar.Time);
      --  Callback on sources from the list of dependencies

      function Only_Ada_Mains (Prj : GPR2.Project.View.Object) return Boolean;
      --  Checks that all mains of given project are Ada sources

      function Locate_Source
        (File : GPR2.Path_Name.Object) return GPR2.Project.Source.Object;
      --  Locates given file by its simple name in the project tree and returns
      --  corresponding source object.

      function Not_Empty_Dir (Dir : GPR2.Path_Name.Object) return Boolean;
      --  Returns true if given directory has at least one file. Used as
      --  a workaround for crash on an empty directory in
      --  Libadalang.Auto_Provider.Find_Files_Regexp.

      -------------
      -- Add_Src --
      -------------

      procedure Add_Src
        (Source    : GPR2.Project.Source.Object;
         Index     : GPR2.Unit_Index;
         Timestamp : Ada.Calendar.Time)
      is
         pragma Unreferenced (Index, Timestamp);
      begin
         if not Source.Is_Runtime then
            Store_Source (Source);
         end if;
      end Add_Src;

      --------------------
      -- Only_Ada_Mains --
      --------------------

      function Only_Ada_Mains (Prj : GPR2.Project.View.Object) return Boolean
      is
      begin
         for Main of Prj.Mains loop
            if not Prj.Source (Main.Source).Is_Ada then
               return False;
            end if;
         end loop;

         return True;
      end Only_Ada_Mains;

      -------------------
      -- Locate_Source --
      -------------------

      function Locate_Source
        (File : GPR2.Path_Name.Object) return GPR2.Project.Source.Object
      is
         File_In_Prj : constant GPR2.Path_Name.Object :=
           My_Project.Tree.Get_File (GPR2.Simple_Name (File.Name));
         Src_Obj     : GPR2.Project.Source.Object;
      begin
         if File_In_Prj.Is_Defined then
            for V of My_Project.Tree loop
               Src_Obj := V.Source (File_In_Prj);
               if Src_Obj.Is_Defined then
                  return Src_Obj;
               end if;
            end loop;
         end if;

         return GPR2.Project.Source.Undefined;
      end Locate_Source;

      ------------------
      -- Store_Source --
      ------------------

      procedure Store_Source (Source : Project.Source.Object) is
      begin
         Store_Sources_To_Process (String (Source.Path_Name.Simple_Name));
      end Store_Source;

      use Ada.Strings.Unbounded;

      -------------------
      -- Not_Empty_Dir --
      -------------------

      function Not_Empty_Dir (Dir : GPR2.Path_Name.Object) return Boolean is
         Files : File_Array_Access := Read_Dir_Recursive
           (Create (+Dir.Value), Filter => Files_Only);
      begin
         if Files /= null then
            Unchecked_Free (Files);
            return True;
         end if;

         return False;
      end Not_Empty_Dir;

   begin
      if Simple_Project then
         --  Use project attribute CodePeer'File_Patterns if set, otherwise
         --  use a default pattern.

         if Proj.Has_Attribute (File_Patterns_Attr) then
            Attr := Proj.Attribute (File_Patterns_Attr);
            if Attr.Kind = Single then
               Error
                 (String (Proj.Path_Name.Simple_Name)
                  & ": Codepeer.File_Patterns value must be a list");
               raise Parameter_Error;
            end if;

            Patterns := new String_List
              (Attr.Values.First_Index .. Attr.Values.Last_Index);
            for J in Attr.Values.First_Index .. Attr.Values.Last_Index loop
               Patterns (J) := new String'(Attr.Values.Element (J).Text);
            end loop;

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

         declare
            Source_Dirs : GPR2.Path_Name.Set.Object;
            Idx         : Natural := 1;
         begin
            if Recursive_Sources then
               for S of My_Project.Tree.Source_Directories loop
                  if S.Exists and then Not_Empty_Dir (S) then
                     Source_Dirs.Append (S);
                  end if;
               end loop;
            else
               for S of My_Project.Tree.Root_Project.Source_Directories loop
                  if S.Exists and then Not_Empty_Dir (S) then
                     Source_Dirs.Append (S);
                  end if;
               end loop;
            end if;

            Directories := new File_Array (1 .. Integer (Source_Dirs.Length));
            for S of Source_Dirs loop
               Directories (Idx) := Create (+S.Value);
               Idx := Idx + 1;
            end loop;

            Source_Dirs.Clear;
         end;

         Files := Find_Files_Regexp
           (Name_Pattern =>
              Compile (To_String (Pattern),
                       Glob           => True,
                       Case_Sensitive => False),
            Directories => Directories.all);

         Unchecked_Free (Directories);

         for F of Files.all loop
            Store_Sources_To_Process (F.Display_Full_Name);
         end loop;

         My_Project.Files := Files;

         return;

      end if;

      if (not No_Argument_File_Specified and then not U_Option_Set)
        or else File_List_Specified
      then
         return;
      end if;

      if U_Option_Set then

         if Main_Unit.Is_Empty then
            --  No argument sources, -U specified. Process recursively
            --  all sources.
            My_Project.Tree.For_Each_Source
              (Action   => Store_Source'Access,
               Language => Ada_Language);
         else
            --  Argument source(s) specified, -U specified. Process closure
            --  of all specified sources.

            --  ??? Closure computation is based upon the current implmentation
            --  of GPR2.Project.Source.Dependencies with Closure => True.
            --  It is to be changed to more closely represent the notion,
            --  of closure, and desired behaviour will be achieved
            --  by setting dedicated Recursive parameter.

            for MU of Main_Unit loop
               declare
                  Src_Obj : constant GPR2.Project.Source.Object :=
                    Locate_Source (MU);
               begin
                  if Src_Obj.Is_Defined then
                     Src_Obj.Dependencies
                       (GPR2.No_Index, Add_Src'Access, Closure => True);
                  else
                     Error (String (MU.Name) & " not found");
                  end if;
               end;
            end loop;

         end if;
      else

         if Recursive_Sources then

            if My_Project.Tree.Root_Project.Has_Mains and then
              Only_Ada_Mains (My_Project.Tree.Root_Project)
            then
               --  No argument sources, no -U/--no-subprojects specified,
               --  root project has mains, all of mains are Ada.
               --  Process closure of those mains.
               for Main of My_Project.Tree.Root_Project.Mains loop
                  My_Project.Tree.Root_Project.Source
                    (Main.Source).Dependencies
                      (GPR2.No_Index, Add_Src'Access, Closure => True);
               end loop;
            else
               --  No argument sources, no -U/--no-subprojects specified,
               --  no mains (or at least one non-Ada main) in root project.
               --  Recursively process all sources.
               My_Project.Tree.For_Each_Source
                 (Action   => Store_Source'Access,
                  Language => Ada_Language);
            end if;
         else

            --  No argument sources, --no-subprojects specified
            for Src of My_Project.Tree.Root_Project.Sources loop
               if not Src.View.Is_Externally_Built and then Src.Is_Ada then
                  Store_Sources_To_Process
                    (String (Src.Path_Name.Simple_Name));
               end if;
            end loop;
         end if;
      end if;
   end Get_Sources_From_Project;

   ----------------------------
   -- Initialize_Environment --
   ----------------------------

   procedure Initialize_Environment is
   begin
      GNATCOLL.Traces.Parse_Config_File;
   end Initialize_Environment;

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
      use GPR2;
      use GPR2.Containers;
      use GPR2.Path_Name;

      RTS : Lang_Value_Map := Lang_Value_Maps.Empty_Map;

      Agg_Context : GPR2.Context.Object;

      KB : constant GPR2.KB.Object :=
        GPR2.KB.Create_Default (GPR2.KB.Default_Flags);
   begin
      if RTS_Path.all /= "" then
         RTS.Insert (GPR2.Ada_Language, RTS_Path.all);
      end if;
      My_Project.Tree.Load_Autoconf
        (Filename          =>
           Create_File
             (Filename_Type (My_Project.Source_Prj.all), No_Resolution),
         Context           => Project_Context,
         Subdirs           =>
           (if Subdir_Name = null then
                 No_Name
            else Name_Type (Subdir_Name.all)),
         Target            => Optional_Name_Type (Target.all),
         Language_Runtimes => RTS,
         Base => KB);

      if not My_Project.Tree.Is_Defined then
         Error ("project not loaded");
      end if;

      pragma Assert (My_Project.Tree.Root_Project.Kind in Aggregate_Kind);

      Agg_Context :=
        GPR2.Project.View.Set.Set.Element
          (My_Project.Tree.Root_Project.Aggregated.First).Context;

      My_Project.Tree.Unload;

      My_Project.Tree.Load_Autoconf
        (Filename          => GPR2.Path_Name.Create_File
             (Filename_Type (Get_Aggregated_Project)),
         Context           => Agg_Context,
         Subdirs           =>
           (if Subdir_Name = null then
                 No_Name
            else Name_Type (Subdir_Name.all)),
         Target            => Optional_Name_Type (Target.all),
         Language_Runtimes => RTS,
         Base => KB);

      My_Project.Tree.Update_Sources
        (Stop_On_Error => False,  With_Runtime => True);

      for Msg_Cur in My_Project.Tree.Log_Messages.Iterate
        (Information => False,
         Warning     => Verbose_Mode)
      loop
         Error (GPR2.Log.Element (Msg_Cur).Format);
      end loop;

   exception
      when GPR2.Project_Error | GPR2.Processing_Error =>
         for Msg_Cur in My_Project.Tree.Log_Messages.Iterate
           (Information => Verbose_Mode)
         loop
            Error (GPR2.Log.Element (Msg_Cur).Format);
         end loop;
         raise Parameter_Error;
   end Load_Aggregated_Project;

   -----------------------
   -- Load_Tool_Project --
   -----------------------

   procedure Load_Tool_Project (My_Project : in out Arg_Project_Type) is
      Aggregated_Prj_Name : GPR2.Path_Name.Object;

      use GPR2;
      use GPR2.Containers;
      use GPR2.Path_Name;

      RTS : Lang_Value_Map := Lang_Value_Maps.Empty_Map;

      Agg_Context : GPR2.Context.Object;

      KB : constant GPR2.KB.Object :=
        GPR2.KB.Create_Default (GPR2.KB.Default_Flags);
   begin
      if RTS_Path.all /= "" then
         RTS.Insert (GPR2.Ada_Language, RTS_Path.all);
      end if;

      My_Project.Tree.Load_Autoconf
        (Filename          =>
           Create_File
             (Filename_Type (My_Project.Source_Prj.all), No_Resolution),
         Context           => Project_Context,
         Subdirs           =>
           (if Subdir_Name = null then
                 No_Name
            else Name_Type (Subdir_Name.all)),
         Target            => Optional_Name_Type (Target.all),
         Language_Runtimes => RTS,
         Base => KB);

      if My_Project.Tree.Root_Project.Kind in Aggregate_Kind then
         Collect_Aggregated_Projects (My_Project.Tree.Root_Project);
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

               Aggregated_Prj_Name := Get_Aggregated_Prj_Src;

               Agg_Context :=
                 GPR2.Project.View.Set.Set.Element
                   (My_Project.Tree.Root_Project.Aggregated.First).Context;

               My_Project.Tree.Unload;

               My_Project.Tree.Load_Autoconf
                 (Filename          => Aggregated_Prj_Name,
                  Context           => Agg_Context,
                  Subdirs           =>
                    (if Subdir_Name = null then
                          No_Name
                     else Name_Type (Subdir_Name.all)),
                  Target            => Optional_Name_Type (Target.all),
                  Language_Runtimes => RTS,
                  Base => KB);
               My_Project.Tree.Update_Sources
                 (Stop_On_Error => False,  With_Runtime => True);

            when others =>
               --  General case - more than one project is aggregated. We have
               --  process them one by one spawning gnatcheck for each project.

               null;
         end case;
      else
         My_Project.Tree.Update_Sources
           (Stop_On_Error => False,  With_Runtime => True);
      end if;

      for Msg_Cur in My_Project.Tree.Log_Messages.Iterate
        (Information => Verbose_Mode,
         Warning     => Verbose_Mode)
      loop
         Error (GPR2.Log.Element (Msg_Cur).Format);
      end loop;

   exception
      when GPR2.Project_Error | GPR2.Processing_Error =>
            for Msg_Cur in My_Project.Tree.Log_Messages.Iterate
              (Information => Verbose_Mode)
            loop
               Error (GPR2.Log.Element (Msg_Cur).Format);
            end loop;
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
      Set_External_Values (My_Project);

      if Aggregated_Project then
         Load_Aggregated_Project (My_Project);
      else
         Load_Tool_Project (My_Project);
      end if;

      if N_Of_Aggregated_Projects > 1 then

         if not Main_Unit.Is_Empty then
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

   ------------------------------
   -- Register_Tool_Attributes --
   ------------------------------

   procedure Register_Tool_Attributes (My_Project : Arg_Project_Type) is
      use GPR2;
      use GPR2.Project.Registry.Attribute;
   begin
      GPR2.Project.Registry.Pack.Add
        (+"Codepeer", GPR2.Project.Registry.Pack.Everywhere);
      Add
        (File_Patterns_Attr,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Pack.Check_Attributes (+"Codepeer", False);

      GPR2.Project.Registry.Pack.Add
        (+"Check", GPR2.Project.Registry.Pack.Everywhere);
      Add
        (Default_Switches_Attr,
         Index_Type           => Language_Index,
         Value                => List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Pack.Check_Attributes (+"Check");
   end Register_Tool_Attributes;

   -------------------------
   -- Set_External_Values --
   -------------------------

   procedure Set_External_Values (My_Project : Arg_Project_Type) is
      GPR_TOOL_Set : Boolean := False;
      use GPR2;
   begin
      for Var of X_Vars loop
         Project_Context.Include
           (Name_Type (Var.Var_Name.all),
            Var.Var_Value.all);

         if Var.Var_Name.all = "GPR_TOOL" then
            GPR_TOOL_Set := True;
         end if;
      end loop;

      --  Set GPR_TOOL, if needed

      if not Ada.Environment_Variables.Exists ("GPR_TOOL")
        and then not GPR_TOOL_Set
      then
         Project_Context.Include ("GPR_TOOL", "gnatcheck");
      end if;
   end Set_External_Values;

   ----------------------------
   -- Set_Global_Result_Dirs --
   ----------------------------

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type) is
      use GPR2;

      Cur_Dir : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create_Directory
          (GPR2.Filename_Type
              (GNAT.Directory_Operations.Get_Current_Dir));

      Dir : constant String :=
        (if not No_Object_Dir and then Gnatcheck_Prj.Is_Specified then
           (if My_Project.Tree.Root_Project.Kind in K_Abstract |
                                                    K_Aggregate then
               My_Project.Tree.Root_Project.Path_Name.Dir_Name
            else
               My_Project.Tree.Root_Project.Object_Directory.Dir_Name)
         else
            Cur_Dir.Dir_Name);
   begin
      GNAT.OS_Lib.Free (Global_Report_Dir);
      Global_Report_Dir := new String'(Dir);
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
         Gnatcheck.Projects.Main_Unit.Append
           (GPR2.Path_Name.Create_File (GPR2.Filename_Type (Unit_Name)));
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

   --------------------------
   -- Process_Rule_Options --
   --------------------------

   type Option_Kind is (File, Option);

   type Option_Record is record
      Kind  : Option_Kind;
      Value : Unbounded_String;
   end record;

   package Vector_Options is new
     Ada.Containers.Vectors (Positive, Option_Record);

   Rule_Options : Vector_Options.Vector;

   procedure Process_Rule_Options is
      use Ada.Strings.Unbounded;
   begin
      for O of Rule_Options loop
         case O.Kind is
            when File =>
               Process_Rule_File (To_String (O.Value));
            when Option =>
               Process_Rule_Option (To_String (O.Value), Defined_At => "");
         end case;
      end loop;
   end Process_Rule_Options;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (My_Project  : in out Arg_Project_Type;
      First_Pass  : Boolean    := False;
      Parser      : Opt_Parser := Command_Line_Parser;
      In_Switches : Boolean    := False)
   is
      procedure Process_Sections;
      --  Processes the 'rules' section.

      procedure Process_Sections is
         use Ada.Strings.Unbounded;
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
                  Rule_Options.Append
                    (Option_Record'(File,
                      To_Unbounded_String (Parameter (Parser => Parser))));

                  if not More_Then_One_Rule_File_Set then
                     Rule_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     More_Then_One_Rule_File_Set := True;
                  else
                     Free (Rule_File_Name);
                  end if;

               when others =>
                  Rule_Options.Append
                    (Option_Record'(Option,
                      To_Unbounded_String
                        (Ada.Strings.Fixed.Trim
                           (Full_Switch (Parser => Parser),
                         Ada.Strings.Both))));
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
              "-no-subprojects "          &
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
                     Gnatcheck.Projects.Recursive_Sources := True;
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

                  elsif Full_Switch (Parser => Parser) = "-rules-dir"
                    and then not Legacy
                  then
                     Additional_Rules_Dirs.Append
                       (Parameter (Parser => Parser));
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

                  elsif Full_Switch (Parser => Parser) = "-no-subprojects" then
                     if not In_Project_File or else not U_Option_Set then
                        Recursive_Sources := False;
                     end if;
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

      if not Subprocess_Mode then
         Ada.Directories.Create_Path (Global_Report_Dir.all);
         Gnatcheck.Output.Set_Report_Files;
      end if;

   end Check_Parameters;

end Gnatcheck.Projects;
