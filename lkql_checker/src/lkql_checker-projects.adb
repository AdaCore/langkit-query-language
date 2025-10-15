--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;         use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

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
with GPR2.Options;
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
with GNATCOLL.Traces;

with Rule_Commands; use Rule_Commands;

package body Lkql_Checker.Projects is

   Project_File_Set : Boolean := False;
   Project_Options  : GPR2.Options.Object;

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

   ------------------------------
   -- External variables table --
   ------------------------------

   X_Vars : GPR2.Containers.Value_Set;

   ----------------------------
   -- GPR2 messages reporter --
   ----------------------------

   type Gnatcheck_Reporter is new GPR2.Reporter.Object with null record;

   overriding
   procedure Internal_Report
     (Self : in out Gnatcheck_Reporter; Message : GPR2.Message.Object);

   overriding
   function Verbosity
     (Self : Gnatcheck_Reporter) return GPR2.Reporter.Verbosity_Level
   is (case Arg.Project_Verbosity.Get is
         when 0      => GPR2.Reporter.No_Warnings,
         when 1      => GPR2.Reporter.Regular,
         when 2      => GPR2.Reporter.Verbose,
         when others => raise Constraint_Error with "should not happen");

   overriding
   function User_Verbosity
     (Self : Gnatcheck_Reporter) return GPR2.Reporter.User_Verbosity_Level
   is (case Arg.Project_Verbosity.Get is
         when 0      => GPR2.Reporter.Important_Only,
         when 1      => GPR2.Reporter.Regular,
         when 2      => GPR2.Reporter.Verbose,
         when others => raise Constraint_Error with "should not happen");

   Gpr2_Reporter : Gnatcheck_Reporter;
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
     (Self : in out Gnatcheck_Reporter; Message : GPR2.Message.Object) is
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
           (Message, Location => Source_Prj (My_Project) & ":1:1");
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
   with Pre => Arg.Aggregated_Project;
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
      if not Arg.Debug_Mode.Get then
         Delete_File (Gprbuild, Success);
         Delete_File (Gprbuild & ".out", Success);
      end if;
   end Clean_Up;

   --------------------------
   -- Extract_Tool_Options --
   --------------------------

   procedure Extract_Tool_Options (My_Project : in out Arg_Project_Type) is
      use GPR2;

      Proj     : constant GPR2.Project.View.Object :=
        My_Project.Tree.Namespace_Root_Projects.First_Element;
      Ada_Idx  : constant GPR2.Project.Attribute_Index.Object :=
        GPR2.Project.Attribute_Index.Create (Ada_Language);
      List_Val : GNAT.OS_Lib.Argument_List_Access;

      function Load_List_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id; Indexed : Boolean := False)
         return GNAT.OS_Lib.Argument_List_Access;
      --  Load the attribute designated by ``Attr_Id`` in the project ``Proj``
      --  as a list value, allocating and returning an ``Argument_List_Access``
      --  that the caller must free after usage.
      --  ``Indexed`` indicates whether the attribute ``Attr_Id`` is an indexed
      --  attribute, if ``True`` this procedure will look at the "ada" index of
      --  this attribute. See ``GPR2.Project.Attribute_Index`` package for more
      --  information about attribute indexes.

      function Load_Single_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id) return String
      is (Proj.Attribute (Attr_Id).Value.Text);
      --  Load the attribute designated by ``Attr_Id`` in the project ``Proj``
      --  as a single value, returning is as a ``String``.

      function Load_List_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id; Indexed : Boolean := False)
         return GNAT.OS_Lib.Argument_List_Access
      is
         Attr : constant GPR2.Project.Attribute.Object :=
           (if Indexed
            then Proj.Attribute (Attr_Id, Ada_Idx)
            else Proj.Attribute (Attr_Id));
         Res  : GNAT.OS_Lib.Argument_List_Access;
      begin
         Res :=
           new String_List (Attr.Values.First_Index .. Attr.Values.Last_Index);
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index loop
            Res (J) := new String'(Attr.Values.Element (J).Text);
         end loop;
         return Res;
      end Load_List_Attribute;

   begin
      --  Process the rule list
      if Proj.Has_Attribute (Rules_Attr) then
         List_Val := Load_List_Attribute (Rules_Attr);
         for Rule of List_Val.all loop
            Add_Rule_By_Name (Rule.all, Prepend => True);
         end loop;
         Free (List_Val);
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
         List_Val := Load_List_Attribute (Lkql_Path_Attr);
         for Path of List_Val.all loop
            Additional_Lkql_Paths.Append
              (if Is_Absolute_Path (Path.all)
               then Path.all
               else Checker_Prj.Get_Project_Relative_File (Path.all));
         end loop;
      end if;

      --  Process additional GNATcheck switches
      if Proj.Has_Attribute (Switches_Attr, Ada_Idx) then
         List_Val := Load_List_Attribute (Switches_Attr, Indexed => True);
         Scan_Arguments (Args => List_Val);
         Free (List_Val);
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
      --  Namespece_Root_Projects will have just one element, this root project
      --  * if the root project is an aggregate project with just one element
      --  then Namespace_Root_Projects.First_Element will return it
      --  * if the root project is an aggregate with several subprojects then
      --  they are loaded individually using Load_Aggregated_Project and so
      --  we're similar to case one
      Root : constant GPR2.Project.View.Object :=
        My_Project.Tree.Namespace_Root_Projects.First_Element;

   begin
      if (Argument_File_Specified and then not Arg.Transitive_Closure.Get)
        or else Arg.Source_Files_Specified
      then
         return;
      end if;

      if Arg.Transitive_Closure.Get then
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
               Root_Project_Only => Arg.No_Subprojects.Get,
               Externally_Built  => False);
         end if;
      else
         if not Arg.No_Subprojects.Get then
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
      use Lkql_Checker.Projects.Aggregate;

      Conf_Obj : GPR2.Project.Configuration.Object;

      Agg_Context : GPR2.Context.Object;

   begin
      Load_Tool_Project (My_Project, Load_Sources => False);

      pragma Assert (My_Project.Tree.Root_Project.Kind in Aggregate_Kind);

      Agg_Context :=
        My_Project.Tree.Namespace_Root_Projects.First_Element.Context;

      Conf_Obj := My_Project.Tree.Configuration;
      My_Project.Tree.Unload;

      --  Amend the project options to load the aggregated project
      Project_Options.Add_Switch
        (GPR2.Options.P, Get_Aggregated_Project, Override => True);

      for C in Agg_Context.Iterate loop
         Project_Options.Add_Switch
           (GPR2.Options.X,
            String (External_Name_Value_Map_Package.Key (C))
            & "="
            & External_Name_Value_Map_Package.Element (C));
      end loop;

      if not My_Project.Tree.Load
               (Project_Options,
                Reporter     => Gpr2_Reporter,
                With_Runtime => True,
                Config       => Conf_Obj)
      then
         if not My_Project.Tree.Has_Runtime_Project then
            My_Project.Error ("no runtime information found");
         end if;

         Error ("""" & Get_Aggregated_Project & """ processing failed");

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
      use Ada.Strings.Unbounded;
   begin
      --  In case of autoconf, restrict to the Ada language

      My_Project.Tree.Restrict_Autoconf_To_Languages
        (Language_Id_Set.To_Set (GPR2.Ada_Language));

      --  Apply the options

      if My_Project.Source_Prj /= null then
         Project_Options.Add_Switch
           (GPR2.Options.P, My_Project.Source_Prj.all);
      end if;

      if My_Project.Source_CGPR /= null then
         Project_Options.Add_Switch
           (GPR2.Options.Config, My_Project.Source_CGPR.all);
      end if;

      if Subdir_Name /= "" then
         Project_Options.Add_Switch (GPR2.Options.Subdirs, Subdir_Name);
      end if;

      if RTS_Path /= Null_Unbounded_String then
         Project_Options.Add_Switch (GPR2.Options.RTS, To_String (RTS_Path));
      end if;

      if Target /= Null_Unbounded_String then
         Project_Options.Add_Switch (GPR2.Options.Target, To_String (Target));
      end if;

      if Arg.Follow_Symbolic_Links.Get then
         Project_Options.Add_Switch (GPR2.Options.Resolve_Links);
      end if;

      if Should_Use_Codepeer_Target then
         GPR2.KB.Set_Default_Target ("codepeer");
      end if;

      if not My_Project.Tree.Load
               (Project_Options,
                Reporter         => Gpr2_Reporter,
                Absent_Dir_Error => GPR2.No_Error,
                With_Runtime     => True)
      then
         raise Parameter_Error;
      end if;

      if not My_Project.Tree.Languages.Contains (GPR2.Ada_Language) then
         My_Project.Error ("project has no Ada sources, processing failed");
         raise Parameter_Error;

      --  Check that an Ada runtime has been found
      elsif not My_Project.Tree.Has_Runtime_Project then
         My_Project.Error ("cannot load the Ada runtime, processing failed");
         raise Parameter_Error;
      end if;

      --  Use the runtime path provided through the project file
      if RTS_Path = Null_Unbounded_String
        and then My_Project.Tree.Runtime (Ada_Language) /= ""
      then
         RTS_Path :=
           To_Unbounded_String
             (String (My_Project.Tree.Runtime (Ada_Language)));
      end if;

      --  Use the target specified through the project file or the config file
      if Target = Null_Unbounded_String then
         Target := To_Unbounded_String (String (My_Project.Tree.Target));
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

   --------------------------
   -- Process_Project_File --
   --------------------------

   procedure Process_Project_File (My_Project : in out Arg_Project_Type'Class)
   is
   begin
      Set_External_Values (My_Project);

      if Arg.Aggregated_Project then
         Load_Aggregated_Project (My_Project);
      else
         Load_Tool_Project (My_Project);
      end if;

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

      if Arg.Text_Report_Enabled then
         Report ("");
         Report ("Processing aggregated project " & Aggregated_Prj_Name);
         Report ("Expected report file: " & Expected_Text_Out_File);
      end if;

      if Arg.XML_Report_Enabled then
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
         & "GNATcheck in this project.");
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

   -------------------------
   -- Set_External_Values --
   -------------------------

   procedure Set_External_Values (My_Project : Arg_Project_Type) is
      GPR_TOOL_Set : Boolean := False;
      use GPR2;
   begin
      --  Set GPR_TOOL, if needed

      for Cursor in Project_Options.Context.Iterate loop
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
         Project_Options.Add_Switch (GPR2.Options.X, "GPR_TOOL=gnatcheck");
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
          (if not Arg.No_Object_Dir.Get and then Checker_Prj.Is_Specified
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

   ---------------------
   -- Set_Subdir_Name --
   ---------------------

   function Subdir_Name return String is
      use Ada.Strings.Unbounded;
   begin
      if Arg.Subdirs.Get = Null_Unbounded_String then
         return "gnatcheck";
      else
         return
           To_String (Arg.Subdirs.Get)
           & GNAT.OS_Lib.Directory_Separator
           & "gnatcheck";
      end if;
   end Subdir_Name;

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

   -----------------
   -- Source_CGPR --
   -----------------

   function Source_CGPR (My_Project : Arg_Project_Type) return String is
   begin
      if not Is_Specified (My_Project) or else My_Project.Source_CGPR = null
      then
         return "";
      elsif My_Project.Tree.Is_Defined then
         return
           My_Project
             .Tree
             .Configuration
             .Corresponding_View
             .Path_Name
             .String_Value;
      else
         return My_Project.Source_CGPR.all;
      end if;
   end Source_CGPR;

   -----------------------------
   -- Store_External_Variable --
   -----------------------------

   procedure Append_Variables
     (Args : in out Argument_List; Last : in out Natural) is
   begin
      for Var of X_Vars loop
         Last := Last + 1;
         Args (Last) := new String'("-X" & Var);
      end loop;
   end Append_Variables;

   -----------------------------
   -- Store_External_Variable --
   -----------------------------

   procedure Store_External_Variable (Var : String) is
   begin
      X_Vars.Include (Var);
      Project_Options.Add_Switch (GPR2.Options.X, Var);
   end Store_External_Variable;

   ---------------------
   -- Store_Main_Unit --
   ---------------------

   procedure Store_Main_Unit (Unit_Name : String) is
   begin
      Lkql_Checker.Projects.Main_Unit.Include (GPR2.Filename_Type (Unit_Name));
   end Store_Main_Unit;

   --------------------------
   -- Store_Project_Source --
   --------------------------

   procedure Store_Project_Source
     (My_Project : in out Arg_Project_Type; Project_File_Name : String)
   is
      Ext : constant String :=
        (if Has_Suffix (Project_File_Name, Suffix => ".gpr")
         then ""
         else ".gpr");

   begin
      if Project_File_Set then
         Error ("cannot have several project files specified");
         raise Parameter_Error;
      else
         Project_File_Set := True;
      end if;

      My_Project.Source_Prj := new String'(Project_File_Name & Ext);
   end Store_Project_Source;

   -----------------------
   -- Store_CGPR_Source --
   -----------------------

   procedure Store_CGPR_Source
     (My_Project : in out Arg_Project_Type; CGPR_File_Name : String) is
   begin
      Project_Options.Add_Switch (GPR2.Options.Config, CGPR_File_Name);
   end Store_CGPR_Source;

   -------------------------------------
   -- Aggregate_Project_Report_Header --
   -------------------------------------

   procedure Aggregate_Project_Report_Header (My_Project : Arg_Project_Type) is
   begin
      if Arg.XML_Report_Enabled then
         XML_Report ("<?xml version=""1.0""?>");
         XML_Report_No_EOL ("<gnatcheck-report");

         if Checker_Prj.Is_Specified then
            XML_Report (" project=""" & Checker_Prj.Source_Prj.all & """>");
         else
            XML_Report (">");
         end if;
      end if;

      Lkql_Checker.Diagnoses.Print_Report_Header;

      if Arg.Text_Report_Enabled then
         Report ("");
         Report ("Argument project is an aggregate project");
         Report ("Aggregated projects are processed separately");
      end if;

      if Arg.XML_Report_Enabled then
         XML_Report ("<aggregated-project-reports>", Indent_Level => 1);
      end if;
   end Aggregate_Project_Report_Header;

   ------------------------------------
   -- Close_Aggregate_Project_Report --
   ------------------------------------

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type) is
   begin
      if Arg.XML_Report_Enabled then
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
      if Arg.Text_Report_Enabled then
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

      if Arg.XML_Report_Enabled then
         XML_Report
           ("<exit-code>" & Image (Exit_Code) & "</exit-code>",
            Indent_Level => 3);

         XML_Report ("</aggregated-project>", Indent_Level => 2);
      end if;
   end Report_Aggregated_Project_Exit_Code;

   --------------------------
   -- Process_Rule_Options --
   --------------------------

   procedure Process_Rule_Options is
      use Ada.Strings.Unbounded;
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

   ----------------------------
   -- Add_Legacy_Rule_Option --
   ----------------------------

   procedure Add_Legacy_Rule_Option (Opt : String; Prepend : Boolean := False)
   is
      use Ada.Strings.Unbounded;

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
      use Ada.Strings.Unbounded;

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
      use Ada.Strings.Unbounded;

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
      use Ada.Strings.Unbounded;
   begin
      return
        Rule_Options.Is_Empty
        and then LKQL_Rule_File_Name = Null_Unbounded_String;
   end Is_Rule_Options_Empty;

   ----------------------
   -- Check_Parameters --
   ----------------------

   procedure Check_Parameters is
      use Ada.Strings.Unbounded;
   begin
      if Arg.Verbose.Get and then not Arg.Aggregated_Project then
         --  When processing aggregated projects one by one, we want
         --  Verbose_Mode to print this only in the outer invocation.
         Print_Version_Info;
      end if;

      --  We generate the rule help unconditionally

      if Arg.List_Rules.Get and then not Arg.Aggregated_Project then
         Rules_Help;
      end if;

      if Arg.List_Rules_XML.Get and then not Arg.Aggregated_Project then
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

      if Arg.List_Rules.Get or else Arg.List_Rules_XML.Get then
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
        new String'(Global_Report_Dir.all & Checker_Config_File.all);

      Analyze_Compiler_Output :=
        Use_gnaty_Option
        or Use_gnatw_Option
        or Check_Restrictions
        or Arg.Check_Semantic.Get;

      --  If GNATcheck is in KP mode and there is a command line specified KP
      --  version, we have to iterate over all implemented rules to enable
      --  those which match the version.
      if Gnatkp_Mode and then Arg.KP_Version.Get /= Null_Unbounded_String then
         for Rule_Cursor in All_Rules.Iterate loop
            declare
               Id       : constant Rule_Id := Rule_Map.Key (Rule_Cursor);
               Rule     : constant Rule_Info := All_Rules (Rule_Cursor);
               Instance : Rule_Instance_Access;
            begin
               if Rule.Impact /= null
                 and then Match
                            (To_String (Arg.KP_Version.Get), Rule.Impact.all)
               then
                  if Rule.Target /= null
                    and then Target /= Null_Unbounded_String
                    and then not Match (To_String (Target), Rule.Target.all)
                  then
                     if not Arg.Quiet_Mode then
                        Info
                          (Ada.Strings.Unbounded.To_String (Rule.Name)
                           & " disabled, target does not match");
                     end if;
                  else
                     if not Arg.Quiet_Mode then
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
         if Gnatkp_Mode and then Arg.KP_Version.Get /= Null_Unbounded_String
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
