--
--  Copyright (C) 2005-2026, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNAT.Directory_Operations;
with GNAT.Regexp; use GNAT.Regexp;

with Lkql_Checker.Compiler;         use Lkql_Checker.Compiler;
with Lkql_Checker.Diagnoses;
with Lkql_Checker.Ids;              use Lkql_Checker.Ids;
with Lkql_Checker.Options;          use Lkql_Checker.Options;
with Lkql_Checker.Output;           use Lkql_Checker.Output;
with Lkql_Checker.Projects.Aggregate;
with Lkql_Checker.Rules;            use Lkql_Checker.Rules;
with Lkql_Checker.Rules.Rule_Table; use Lkql_Checker.Rules.Rule_Table;
with Lkql_Checker.Source_Table;     use Lkql_Checker.Source_Table;
with Lkql_Checker.String_Utilities; use Lkql_Checker.String_Utilities;

with GPR2;
with GPR2.Build.Compilation_Unit;
pragma Warnings (Off, ".* is not referenced");
--  GPR2.Project.View has only limited view on Source.Sets, so we need
--  an explicit with here to be able to use View.Sources
with GPR2.Build.Source.Sets;
pragma Warnings (On);
with GPR2.Context;
with GPR2.KB;
with GPR2.Log;
with GPR2.Message;
with GPR2.Path_Name;
with GPR2.Project.Attribute;
with GPR2.Project.Attribute_Index;
with GPR2.Project.Configuration;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Registry.Attribute.Description;
with GPR2.Project.Registry.Pack;
with GPR2.Project.Registry.Pack.Description;
with GPR2.Reporter.Console;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;
with GNATCOLL.Strings;   use GNATCOLL.Strings;

with Rule_Commands; use Rule_Commands;

package body Lkql_Checker.Projects is

   Rules_Attr            : constant GPR2.Q_Attribute_Id :=
     (GPR2."+" ("Check"), GPR2."+" ("Rules"));
   Rule_File_Attr        : constant GPR2.Q_Attribute_Id :=
     (GPR2."+" ("Check"), GPR2."+" ("Rule_File"));
   Lkql_Path_Attr        : constant GPR2.Q_Attribute_Id :=
     (GPR2."+" ("Check"), GPR2."+" ("Lkql_Path"));
   Default_Switches_Attr : constant GPR2.Q_Attribute_Id :=
     (GPR2."+" ("Check"), GPR2."+" ("Default_Switches"));
   Switches_Attr         : constant GPR2.Q_Attribute_Id :=
     (GPR2."+" ("Check"), GPR2."+" ("Switches"));
   File_Patterns_Attr    : constant GPR2.Q_Attribute_Id :=
     (GPR2."+" ("CodePeer"), GPR2."+" ("File_Patterns"));

   ----------------------------
   -- GPR2 messages reporter --
   ----------------------------

   type Lkql_Checker_Reporter is new GPR2.Reporter.Object with null record;

   overriding
   procedure Internal_Report
     (Self : in out Lkql_Checker_Reporter; Message : GPR2.Message.Object);

   overriding
   function Verbosity
     (Self : Lkql_Checker_Reporter) return GPR2.Reporter.Verbosity_Level
   is (case GPR_Args.Project_Verbosity.Get is
         when 0      => GPR2.Reporter.No_Warnings,
         when 1      => GPR2.Reporter.Regular,
         when 2      => GPR2.Reporter.Verbose,
         when others => raise Constraint_Error with "should not happen");

   overriding
   function User_Verbosity
     (Self : Lkql_Checker_Reporter) return GPR2.Reporter.User_Verbosity_Level
   is (case GPR_Args.Project_Verbosity.Get is
         when 0      => GPR2.Reporter.Important_Only,
         when 1      => GPR2.Reporter.Regular,
         when 2      => GPR2.Reporter.Verbose,
         when others => raise Constraint_Error with "should not happen");

   Gpr2_Reporter : Lkql_Checker_Reporter;
   --  Make libgpr2 report messages using the proper ``Lkql_Checker.Output``
   --  API.

   function Report_Missing_File (Log : String) return Boolean
   is (Index (Log, "source file") /= 0 and then Index (Log, "not found") /= 0);
   --  Checks if Log reports about a missing source file.

   ------------
   -- Report --
   ------------

   overriding
   procedure Internal_Report
     (Self : in out Lkql_Checker_Reporter; Message : GPR2.Message.Object) is
   begin
      --  Check if the message is reporting about a missing file
      if not Missing_File_Detected
        and then Report_Missing_File (Message.Message)
      then
         Missing_File_Detected := True;
      end if;

      --  Then just print the formatted message
      Print (Message.Format);
   end Internal_Report;

   -----------
   -- Error --
   -----------

   procedure Error (My_Project : Arg_Project_Type; Message : String) is
   begin
      if My_Project.Is_Specified then
         Lkql_Checker.Output.Error
           (Message,
            Location =>
              String (My_Project.Options.Project_File.Simple_Name) & ":1:1");
      else
         Lkql_Checker.Output.Error (Message);
      end if;
   end Error;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Load_Tool_Project
     (My_Project   : in out Arg_Project_Type'Class;
      Load_Sources : Boolean := True);
   --  Loads argument project

   procedure Load_Aggregated_Project
     (My_Project : in out Arg_Project_Type'Class)
   with Pre => GPR_Args.Aggregated_Project;
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
      if not Tool_Args.Debug_Mode.Get then
         Delete_File (Gprbuild, Success);
         Delete_File (Gprbuild & ".out", Success);
      end if;
   end Clean_Up;

   --------------------------
   -- Extract_Tool_Options --
   --------------------------

   procedure Extract_Tool_Options (My_Project : Arg_Project_Type) is
      use GPR2;

      Proj    : constant GPR2.Project.View.Object :=
        My_Project.Tree.Namespace_Root_Projects.First_Element;
      Ada_Idx : constant GPR2.Project.Attribute_Index.Object :=
        GPR2.Project.Attribute_Index.Create (Ada_Language);

      function Load_List_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id; Indexed : Boolean := False)
         return String_Vector;
      --  Load the attribute designated by ``Attr_Id`` in the project ``Proj``
      --  as a list value, allocating and returning an ``Argument_List_Access``
      --  that the caller must free after usage.
      --  ``Indexed`` indicates whether the attribute ``Attr_Id`` is an indexed
      --  attribute, if ``True`` this procedure will look at the "ada" index of
      --  this attribute. See ``GPR2.Project.Attribute_Index`` package for more
      --  information about attribute indexes.

      function To_XString_Array (Vec : String_Vector) return XString_Array;

      function Load_Single_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id) return String
      is (Proj.Attribute (Attr_Id).Value.Text);
      --  Load the attribute designated by ``Attr_Id`` in the project ``Proj``
      --  as a single value, returning is as a ``String``.

      function Load_List_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id; Indexed : Boolean := False)
         return String_Vector
      is
         Attr : constant GPR2.Project.Attribute.Object :=
           (if Indexed
            then Proj.Attribute (Attr_Id, Ada_Idx)
            else Proj.Attribute (Attr_Id));
         Res  : String_Vector;
      begin
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index loop
            Res.Append (String (Attr.Values.Element (J).Text));
         end loop;
         return Res;
      end Load_List_Attribute;

      function To_XString_Array (Vec : String_Vector) return XString_Array is
         Res : XString_Array (Vec.First_Index .. Vec.Last_Index);
      begin
         for I in Res'Range loop
            Res (I) := To_XString (Vec (I));
         end loop;
         return Res;
      end To_XString_Array;

   begin
      --  Process the rule list
      if Proj.Has_Attribute (Rules_Attr) then
         for Rule of Load_List_Attribute (Rules_Attr) loop
            Add_Rule_By_Name (Rule, Prepend => True);
         end loop;
      end if;

      --  Process the LKQL rule file
      if Proj.Has_Attribute (Rule_File_Attr) then
         declare
            Rule_File : constant String :=
              Load_Single_Attribute (Rule_File_Attr);
         begin
            Set_LKQL_Rule_File (Rule_File, True);
         end;
      end if;

      --  Process the LKQL path
      if Proj.Has_Attribute (Lkql_Path_Attr) then
         for Path of Load_List_Attribute (Lkql_Path_Attr) loop
            Additional_Lkql_Paths.Append
              (if Is_Absolute_Path (Path)
               then Path
               else Checker_Prj.Get_Project_Relative_File (Path));
         end loop;
      end if;

      --  Process additional GNATcheck switches
      if Proj.Has_Attribute (Switches_Attr, Ada_Idx) then
         Scan_Tool_Arguments
           (Args              =>
              To_XString_Array
                (Load_List_Attribute (Switches_Attr, Indexed => True)),
            From_Project_File => True);
      end if;
   end Extract_Tool_Options;

   ------------------------------
   -- Get_Sources_From_Project --
   ------------------------------

   procedure Get_Sources_From_Project (My_Project : in out Arg_Project_Type) is
      use type GPR2.Language_Id;

      function Only_Ada_Mains (Prj : GPR2.Project.View.Object) return Boolean;
      --  Returns whether the provided ``Prj`` project view defines only Ada
      --  mains.

      procedure Store_Source (Unit : GPR2.Build.Compilation_Unit.Object);
      --  Callback used to store sources

      --------------------
      -- Only_Ada_Mains --
      --------------------

      function Only_Ada_Mains (Prj : GPR2.Project.View.Object) return Boolean
      is
         Src : GPR2.Build.Source.Object;
         CU  : GPR2.Build.Compilation_Unit.Unit_Location;

      begin
         for C in Prj.Mains.Iterate loop
            CU :=
              GPR2.Build.Compilation_Unit.Unit_Location_Vectors.Element (C);
            Src := CU.View.Source (CU.Source.Simple_Name);

            if Src.Language /= GPR2.Ada_Language then
               return False;
            end if;
         end loop;

         return True;
      end Only_Ada_Mains;

      ------------------
      -- Store_Source --
      ------------------

      procedure Store_Source (Unit : GPR2.Build.Compilation_Unit.Object) is
         use GPR2.Build.Compilation_Unit.Separate_Maps;
      begin
         for Part in GPR2.S_Spec .. GPR2.S_Body loop
            if Unit.Has_Part (Part) then
               Store_Sources_To_Process
                 (String (Unit.Get (Part).Source.Simple_Name));
            end if;
         end loop;

         if Unit.Has_Part (GPR2.S_Separate) then
            for Sep in Unit.Separates.Iterate loop
               Store_Sources_To_Process
                 (String (Element (Sep).Source.Simple_Name));
            end loop;
         end if;
      end Store_Source;

      --  Get the root project:
      --  * if the root project is a regular project then
      --  Namespace_Root_Projects will have just one element, this root project
      --  * if the root project is an aggregate project with just one element
      --  then Namespace_Root_Projects.First_Element will return it
      --  * if the root project is an aggregate with several subprojects then
      --  they are loaded individually using Load_Aggregated_Project and so
      --  we're similar to case one
      Root : constant GPR2.Project.View.Object :=
        My_Project.Tree.Namespace_Root_Projects.First_Element;

   begin
      if (Argument_File_Specified
          and then not Tool_Args.Transitive_Closure.Get)
        or else Tool_Args.Source_Files_Specified
      then
         return;
      end if;

      if Tool_Args.Transitive_Closure.Get then
         if Main_Unit.Is_Empty then
            --  No argument sources, -U specified. Process recursively
            --  all sources.

            for U of Root.Units loop
               Store_Source (U);
            end loop;
         else
            --  No argument sources, -U specified. Process recursively
            --  all sources.
            My_Project.Tree.For_Each_Ada_Closure
              (Action            => Store_Source'Access,
               Mains             => Main_Unit,
               Root_Project_Only => Tool_Args.No_Subprojects.Get,
               Externally_Built  => False);
         end if;
      else
         if not Tool_Args.No_Subprojects.Get then
            if Root.Has_Mains and then Only_Ada_Mains (Root) then
               --  No argument sources, no -U/--no-subprojects specified,
               --  root project has mains, all of mains are Ada.
               --  Process closure of those mains.

               My_Project.Tree.For_Each_Ada_Closure
                 (Action            => Store_Source'Access,
                  Root_Project_Only => False,
                  Externally_Built  => False);
            else
               --  No argument sources, no -U/--no-subprojects specified,
               --  no mains (or at least one non-Ada main) in root project.
               --  Recursively process all sources.

               for U of Root.Units loop
                  Store_Source (U);
               end loop;
            end if;
         else
            for Src of Root.Sources loop
               if Src.Language = GPR2.Ada_Language then
                  Store_Sources_To_Process
                    (String (Src.Path_Name.Simple_Name));
               end if;
            end loop;
         end if;
      end if;

   exception
      when E : GPR2.Options.Usage_Error =>
         My_Project.Error
           ("libgpr2 usage error: " & Ada.Exceptions.Exception_Message (E));
   end Get_Sources_From_Project;

   ------------------
   -- Is_Specified --
   ------------------

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean is
   begin
      return My_Project.Options.Project_File.Is_Defined;
   end Is_Specified;

   -------------------------------
   -- Get_Project_Relative_File --
   -------------------------------

   function Get_Project_Relative_File
     (My_Project : Arg_Project_Type; Filename : String) return String is
   begin
      if Lkql_Checker.Options.Checker_Prj.Is_Specified
        and then Lkql_Checker.Options.Checker_Prj.Tree.Is_Defined
      then
         return
           Normalize_Pathname
             (GNAT.Directory_Operations.Dir_Name
                (Checker_Prj.Tree.Root_Project.Path_Name.String_Value)
              & Filename);
      else
         return Normalize_Pathname (Filename);
      end if;
   end Get_Project_Relative_File;

   -----------------------------
   -- Load_Aggregated_Project --
   -----------------------------

   procedure Load_Aggregated_Project
     (My_Project : in out Arg_Project_Type'Class)
   is
      use GPR2;
      use GPR2.Containers;

      Conf_Obj    : GPR2.Project.Configuration.Object;
      Agg_Context : GPR2.Context.Object;
   begin
      My_Project.Load_Tool_Project (Load_Sources => False);

      pragma Assert (My_Project.Tree.Root_Project.Kind in Aggregate_Kind);

      Agg_Context :=
        My_Project.Tree.Namespace_Root_Projects.First_Element.Context;

      Conf_Obj := My_Project.Tree.Configuration;
      My_Project.Tree.Unload;

      --  Amend the project options to load the aggregated project
      My_Project.Options.Add_Switch
        (GPR2.Options.P,
         To_String (GPR_Args.Aggregate_Subproject.Get),
         Override => True);

      for C in Agg_Context.Iterate loop
         My_Project.Options.Add_Switch
           (GPR2.Options.X,
            String (External_Name_Value_Map_Package.Key (C))
            & "="
            & External_Name_Value_Map_Package.Element (C));
      end loop;

      if not My_Project.Tree.Load
               (My_Project.Options,
                Reporter     => Gpr2_Reporter,
                With_Runtime => True,
                Config       => Conf_Obj)
      then
         if not My_Project.Tree.Has_Runtime_Project then
            My_Project.Error ("no runtime information found");
         end if;

         Error
           (""""
            & To_String (GPR_Args.Aggregate_Subproject.Get)
            & """ processing failed");

         raise Parameter_Error;
      end if;

      if not My_Project.Tree.Update_Sources (GPR2.Sources_Units) then
         raise Parameter_Error;
      end if;

   exception
      when E : GPR2.Options.Usage_Error =>
         My_Project.Error
           ("libgpr2 usage error: " & Ada.Exceptions.Exception_Message (E));
         raise Parameter_Error;
   end Load_Aggregated_Project;

   -----------------------
   -- Load_Tool_Project --
   -----------------------

   procedure Load_Tool_Project
     (My_Project   : in out Arg_Project_Type'Class;
      Load_Sources : Boolean := True)
   is
      use GPR2;
      use GPR2.Containers;
   begin
      --  In case of autoconf, restrict to the Ada language
      My_Project.Tree.Restrict_Autoconf_To_Languages
        (Language_Id_Set.To_Set (GPR2.Ada_Language));

      --  Always suffix the subdirectories by "gnatcheck"
      if My_Project.Options.Subdirs = Null_Unbounded_String then
         My_Project.Options.Add_Switch
           (GPR2.Options.Subdirs, "gnatcheck", Override => True);
      else
         My_Project.Options.Add_Switch
           (GPR2.Options.Subdirs,
            To_String (My_Project.Options.Subdirs)
            & GNAT.OS_Lib.Directory_Separator
            & "gnatcheck",
            Override => True);
      end if;

      if Should_Use_Codepeer_Target then
         GPR2.KB.Set_Default_Target ("codepeer");
      end if;

      if not My_Project.Tree.Load
               (My_Project.Options,
                Reporter         => Gpr2_Reporter,
                Absent_Dir_Error => GPR2.No_Error,
                With_Runtime     => True)
      then
         raise Parameter_Error;
      end if;

      --  Check that the project contains Ada sources
      if not My_Project.Tree.Languages.Contains (GPR2.Ada_Language) then
         My_Project.Error ("project has no Ada sources, processing failed");
         raise Parameter_Error;

      --  Check that an Ada runtime has been found
      elsif not My_Project.Tree.Has_Runtime_Project then
         My_Project.Error ("cannot load the Ada runtime, processing failed");
         raise Parameter_Error;
      end if;

      if Load_Sources then
         if My_Project.Tree.Root_Project.Kind = K_Aggregate then
            --  We cannot load sources when the number of aggregated projects
            --  is more than one (ambiguities in terms of Ada units may then
            --  arise). Let's check here, and run gnatcheck on
            --  each aggregated project when necessary.

            Lkql_Checker.Projects.Aggregate.Collect_Aggregated_Projects
              (My_Project.Tree);

         end if;

         if not My_Project.Tree.Update_Sources then
            raise Parameter_Error;
         end if;
      end if;

   exception
      when E : GPR2.Options.Usage_Error =>
         My_Project.Error
           ("libgpr2 usage error: " & Ada.Exceptions.Exception_Message (E));
         raise Parameter_Error;
   end Load_Tool_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (My_Project : in out Arg_Project_Type; Options : GPR2.Options.Object) is
   begin
      My_Project.Options := Options;
      Set_External_Values (My_Project);
      if GPR_Args.Aggregated_Project then
         Load_Aggregated_Project (My_Project);
      else
         Load_Tool_Project (My_Project);
      end if;
   end Load_Project;

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

      if Tool_Args.Text_Report_Enabled then
         Report ("");
         Report ("Processing aggregated project " & Aggregated_Prj_Name);
         Report ("Expected report file: " & Expected_Text_Out_File);
      end if;

      if Tool_Args.XML_Report_Enabled then
         XML_Report ("<aggregated-project>", Indent_Level => 2);

         XML_Report
           ("<project-file>" & Aggregated_Prj_Name & "</project-file>",
            Indent_Level => 3);

         XML_Report
           ("<report-file>" & Expected_XML_Out_File & "</report-file>",
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
      GPR2.Project.Registry.Pack.Description.Set_Package_Description
        (+"Codepeer",
         "This package specifies the options used when "
         & "calling the tool 'codepeer'.");
      Add
        (File_Patterns_Attr,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (File_Patterns_Attr,
         "If you want to override ada default file "
         & "extensions (ada, ads, adb, spc & bdy), use this attribute "
         & "which includes a list of file patterns where you can specify "
         & "the following meta characters: * : matches any string of 0 "
         & "or more characters, ? : matches any character, "
         & " [list of chars] : matches any character listed, [char-char] "
         & ": matches any character in given range, [^list of chars] : "
         & "matches any character not listed. These patterns are case "
         & "insensitive.");
      GPR2.Project.Registry.Pack.Check_Attributes (+"Codepeer", False);

      GPR2.Project.Registry.Pack.Add
        (+"Check", GPR2.Project.Registry.Pack.Everywhere);
      GPR2.Project.Registry.Pack.Description.Set_Package_Description
        (+"Check",
         "This package specifies the options used when "
         & "calling the checking tool 'gnatcheck'. Its attribute "
         & "Default_Switches has the same semantics as for the package "
         & "Builder. The first string should always be -rules to specify "
         & "that all the other options belong to the -rules section of "
         & "the parameters to 'gnatcheck'.");
      Add
        (Rules_Attr,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => List,
         Value_Case_Sensitive => False,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Rules_Attr,
         "Value is a list of GNATcheck rule names to enable when running "
         & "GNATcheck on this project.");
      Add
        (Rule_File_Attr,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => Single,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Rule_File_Attr,
         "Value is the name of an LKQL rule file to use when running "
         & "GNATcheck on this project.");
      Add
        (Lkql_Path_Attr,
         Index_Type           => GPR2.Project.Registry.Attribute.No_Index,
         Value                => List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Lkql_Path_Attr,
         "Value is a list of directories to add to the LKQL_PATH environment "
         & "variable when GNATcheck is spawning the LKQL engine. This "
         & "variable is used to resolve module importations in LKQL sources.");
      Add
        (Switches_Attr,
         Index_Type           => Language_Index,
         Value                => List,
         Value_Case_Sensitive => True,
         Is_Allowed_In        => Everywhere);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Switches_Attr,
         "Index is a language name. Value is a "
         & "list of switches to be used when invoking 'gnatcheck' for a "
         & "source of the language.");
      Add_Alias (Name => Default_Switches_Attr, Alias_Of => Switches_Attr);
      GPR2.Project.Registry.Attribute.Description.Set_Attribute_Description
        (Default_Switches_Attr,
         "Index is a language name. Value is a "
         & "list of switches to be used when invoking 'gnatcheck' for a "
         & "source of the language, if there is no applicable attribute "
         & "Switches.");
      GPR2.Project.Registry.Pack.Check_Attributes (+"Check");
   end Register_Tool_Attributes;

   ------------------------
   -- Print_GPR_Registry --
   ------------------------

   procedure Print_GPR_Registry (My_Project : Arg_Project_Type) is
   begin
      My_Project.Options.Print_GPR_Registry;
   end Print_GPR_Registry;

   -------------------------
   -- Set_External_Values --
   -------------------------

   procedure Set_External_Values (My_Project : in out Arg_Project_Type) is
      GPR_TOOL_Set : Boolean := False;
      use GPR2;
   begin
      --  Set GPR_TOOL, if needed

      for Cursor in My_Project.Options.Context.Iterate loop
         if Containers.External_Name_Value_Map_Package.Key (Cursor)
           = "GPR_TOOL"
         then
            GPR_TOOL_Set := True;
            exit;
         end if;
      end loop;

      if not Ada.Environment_Variables.Exists ("GPR_TOOL")
        and then not GPR_TOOL_Set
      then
         My_Project.Options.Add_Switch (GPR2.Options.X, "GPR_TOOL=gnatcheck");
      end if;
   end Set_External_Values;

   ----------------------------
   -- Set_Global_Result_Dirs --
   ----------------------------

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type) is
      use GPR2;

      Cur_Dir : constant GPR2.Path_Name.Object :=
        GPR2.Path_Name.Create_Directory
          (GPR2.Filename_Type (GNAT.Directory_Operations.Get_Current_Dir));

      Dir : constant String :=
        String
          (if not Tool_Args.No_Object_Dir.Get and then Checker_Prj.Is_Specified
           then
             (if My_Project.Tree.Root_Project.Kind
                 not in GPR2.With_Object_Dir_Kind
              then My_Project.Tree.Root_Project.Path_Name.Dir_Name
              else My_Project.Tree.Root_Project.Object_Directory.Dir_Name)
           else Cur_Dir.Dir_Name);
   begin
      GNAT.OS_Lib.Free (Global_Report_Dir);
      Global_Report_Dir := new String'(Dir);
   end Set_Global_Result_Dirs;

   ----------------
   -- Source_Prj --
   ----------------

   function Source_Prj (My_Project : Arg_Project_Type) return String is
   begin
      if My_Project.Is_Specified then
         return String (My_Project.Options.Project_File.Name);
      else
         return "";
      end if;
   end Source_Prj;

   -----------------
   -- Source_CGPR --
   -----------------

   function Source_CGPR (My_Project : Arg_Project_Type) return String is
   begin
      if My_Project.Options.Config_Project.Is_Defined then
         return String (My_Project.Options.Config_Project.Name);
      else
         return "";
      end if;
   end Source_CGPR;

   ------------
   -- Target --
   ------------

   function Target (My_Project : Arg_Project_Type) return String is
      Specified_Target : constant String := String (My_Project.Options.Target);
   begin
      if Specified_Target /= "all" then
         return Specified_Target;
      else
         return String (My_Project.Tree.Target);
      end if;
   end Target;

   -------------
   -- Runtime --
   -------------

   function Runtime (My_Project : Arg_Project_Type) return String is
   begin
      if My_Project.Options.RTS_Map.Contains (GPR2.Ada_Language) then
         return My_Project.Options.RTS_Map.Element (GPR2.Ada_Language);
      else
         return String (My_Project.Tree.Runtime (GPR2.Ada_Language));
      end if;
   end Runtime;

   -----------------
   -- Subdir_Name --
   -----------------

   function Subdir_Name (My_Project : Arg_Project_Type) return String is
   begin
      return To_String (My_Project.Options.Subdirs);
   end Subdir_Name;

   ---------------------------
   -- Follow_Symbolic_Links --
   ---------------------------

   function Follow_Symbolic_Links
     (My_Project : Arg_Project_Type) return Boolean is
   begin
      return My_Project.Options.Resolve_Links;
   end Follow_Symbolic_Links;

   -------------------------------
   -- Append_External_Variables --
   -------------------------------

   procedure Append_External_Variables
     (My_Project : Arg_Project_Type;
      Args       : in out Argument_List;
      Last       : in out Natural)
   is
      use GPR2.Context.Key_Value;

      procedure Place_In_Args (C : Cursor);
      procedure Place_In_Args (C : Cursor) is
      begin
         Last := Last + 1;
         Args (Last) :=
           new String'("-X" & String (Key (C)) & '=' & Element (C));
      end Place_In_Args;
   begin
      My_Project.Options.Context.Iterate (Place_In_Args'Access);
   end Append_External_Variables;

   ---------------------
   -- Store_Main_Unit --
   ---------------------

   procedure Store_Main_Unit (Unit_Name : String) is
   begin
      Lkql_Checker.Projects.Main_Unit.Include (GPR2.Filename_Type (Unit_Name));
   end Store_Main_Unit;

   -------------------------------------
   -- Aggregate_Project_Report_Header --
   -------------------------------------

   procedure Aggregate_Project_Report_Header (My_Project : Arg_Project_Type) is
   begin
      if Tool_Args.XML_Report_Enabled then
         XML_Report ("<?xml version=""1.0""?>");
         XML_Report_No_EOL ("<gnatcheck-report");

         if Checker_Prj.Is_Specified then
            XML_Report (" project=""" & Checker_Prj.Source_Prj & """>");
         else
            XML_Report (">");
         end if;
      end if;

      Lkql_Checker.Diagnoses.Print_Report_Header;

      if Tool_Args.Text_Report_Enabled then
         Report ("");
         Report ("Argument project is an aggregate project");
         Report ("Aggregated projects are processed separately");
      end if;

      if Tool_Args.XML_Report_Enabled then
         XML_Report ("<aggregated-project-reports>", Indent_Level => 1);
      end if;
   end Aggregate_Project_Report_Header;

   ------------------------------------
   -- Close_Aggregate_Project_Report --
   ------------------------------------

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type) is
   begin
      if Tool_Args.XML_Report_Enabled then
         XML_Report ("</aggregated-project-reports>", Indent_Level => 1);
         XML_Report ("</gnatcheck-report>");
      end if;
   end Close_Aggregate_Project_Report;

   -----------------------------------------
   -- Report_Aggregated_Project_Exit_Code --
   -----------------------------------------

   procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Arg_Project_Type; Exit_Code : Integer)
   is
      pragma Unreferenced (Aggregate_Prj);
   begin
      if Tool_Args.Text_Report_Enabled then
         Report
           ("Exit code is"
            & Exit_Code'Img
            & " ("
            & (case Exit_Code is
                 when 0      => "no rule violation detected",
                 when 1      => "rule violation(s) detected",
                 when 2      => "tool failure, results cannot be trusted",
                 when 3      => "no rule check performed",
                 when others => "unknown")
            & ")");
      end if;

      if Tool_Args.XML_Report_Enabled then
         XML_Report
           ("<exit-code>" & Image (Exit_Code) & "</exit-code>",
            Indent_Level => 3);

         XML_Report ("</aggregated-project>", Indent_Level => 2);
      end if;
   end Report_Aggregated_Project_Exit_Code;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is
   begin
      if Tool_Args.Verbose.Get and then not GPR_Args.Aggregated_Project then
         --  When processing aggregated projects one by one, we want
         --  Verbose_Mode to print this only in the outer invocation.
         Print_Version_Info;
      end if;

      --  We generate the rule help unconditionally

      if Tool_Args.List_Rules.Get and then not GPR_Args.Aggregated_Project then
         Rules_Help;
      end if;

      if Tool_Args.List_Rules_XML.Get and then not GPR_Args.Aggregated_Project
      then
         XML_Help;
      end if;

      if In_Aggregate_Project then
         --  We have to skip most of the checks because this call does not do
         --  anything except spawning another gnatcheck for individual projects

         Set_Global_Result_Dirs (Checker_Prj);
         goto Processing_Aggregate_Project;
      end if;

      --  No need to perform similar checks for custom XML file because it can
      --  be set only with turning ON XML output

      if Tool_Args.List_Rules.Get or else Tool_Args.List_Rules_XML.Get then
         Nothing_To_Do := True;
         return;
      end if;

      Read_Args_From_Temp_Storage
        (Duplication_Report => not Is_Specified (Checker_Prj),
         Arg_Project        => Checker_Prj);
      Nothing_To_Do := Last_Source < First_SF_Id;

      if Nothing_To_Do then
         Error ("no existing file to process");
         return;
      end if;

      Lkql_Checker.Projects.Set_Global_Result_Dirs (Checker_Prj);
      Checker_Config_File :=
        new String'
          (Normalize_Pathname
             (Global_Report_Dir.all & Checker_Config_File.all));

      Analyze_Compiler_Output :=
        Use_gnaty_Option
        or Use_gnatw_Option
        or Check_Restrictions
        or Tool_Args.Check_Semantic.Get;

      --  If GNATcheck is in KP mode and there is a command line specified KP
      --  version, we have to iterate over all implemented rules to enable
      --  those which match the version.
      if Mode = Gnatkp_Mode
        and then Tool_Args.KP_Version.Get /= Null_Unbounded_String
      then
         for Rule_Cursor in All_Rules.Iterate loop
            declare
               Id       : constant Rule_Id := Rule_Map.Key (Rule_Cursor);
               Rule     : constant Rule_Info := All_Rules (Rule_Cursor);
               Instance : Rule_Instance_Access;
            begin
               if Rule.Impact /= null
                 and then Match
                            (To_String (Tool_Args.KP_Version.Get),
                             Rule.Impact.all)
               then
                  if Rule.Target /= null
                    and then Checker_Prj.Target /= ""
                    and then not Match (Checker_Prj.Target, Rule.Target.all)
                  then
                     if not Tool_Args.Quiet_Mode then
                        Info
                          (Ada.Strings.Unbounded.To_String (Rule.Name)
                           & " disabled, target does not match");
                     end if;
                  else
                     if not Tool_Args.Quiet_Mode then
                        Info
                          (Ada.Strings.Unbounded.To_String (Rule.Name)
                           & " enabled");
                     end if;

                     Instance := Rule.Create_Instance (Is_Alias => False);
                     Instance.Rule := Id;
                     Instance.Source_Mode := General;
                     Turn_Instance_On (Instance);
                  end if;
               end if;
            end;
         end loop;
      end if;

      Active_Rule_Present := not All_Rule_Instances.Is_Empty;

      if not (Active_Rule_Present or else Analyze_Compiler_Output) then
         if Mode = Gnatkp_Mode
           and then Tool_Args.KP_Version.Get /= Null_Unbounded_String
         then
            Error ("no rule for the given kp-version");
            No_Detectors_For_KP_Version := True;
            return;
         else
            Error ("no rule to check specified");
            raise Parameter_Error;
         end if;
      end if;

      if Exempted_Units /= null then
         Process_Exemptions (Exempted_Units.all);
      end if;

      Total_Sources := Total_Sources_To_Process;

      if Total_Sources = 0 then
         Error ("no existing file to process");
         Nothing_To_Do := True;
         return;
      end if;

      --  If we are here - we have sources to check and rules to apply

      Sources_Left := Total_Sources;

      <<Processing_Aggregate_Project>>

      Ada.Directories.Create_Path (Global_Report_Dir.all);
      Lkql_Checker.Output.Set_Report_Files;
   end Check_Parameters;

end Lkql_Checker.Projects;
