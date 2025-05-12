--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Directories;         use Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.Regexp;       use GNAT.Regexp;
with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Table;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses;
with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Projects.Aggregate;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Source_Table;     use Gnatcheck.Source_Table;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

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

package body Gnatcheck.Projects is

   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

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
     (Self : Gnatcheck_Reporter) return GPR2.Reporter.Verbosity_Level;

   Gpr2_Reporter : Gnatcheck_Reporter;
   --  Make libgpt2 report messages using the proper gnatcheck.Output API

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
      case Message.Level is
         when GPR2.Message.Error =>
            Print (Message.Format);

         when GPR2.Message.Warning =>
            if Verbose_Mode then
               Print (Message.Format);
            end if;

            if not Missing_File_Detected
              and then Report_Missing_File (Message.Message)
            then
               Missing_File_Detected := True;
            end if;

         when others =>
            null;
      end case;
   end Internal_Report;

   ---------------
   -- Verbosity --
   ---------------

   overriding
   function Verbosity
     (Self : Gnatcheck_Reporter) return GPR2.Reporter.Verbosity_Level is
   begin
      return GPR2.Reporter.Regular;
   end Verbosity;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Store_Compiler_Option (Switch : String);
   --  Stores compiler option as is

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
      use GPR2.Project.Registry.Attribute;

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
        (Attr_Id : GPR2.Q_Attribute_Id) return String;
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
         if Attr.Kind /= List then
            Error
              (String (Proj.Path_Name.Simple_Name)
               & ": "
               & Image (Attr_Id)
               & " value must be a list");
            raise Parameter_Error;
         end if;

         Res :=
           new String_List (Attr.Values.First_Index .. Attr.Values.Last_Index);
         for J in Attr.Values.First_Index .. Attr.Values.Last_Index loop
            Res (J) := new String'(Attr.Values.Element (J).Text);
         end loop;

         return Res;
      end Load_List_Attribute;

      function Load_Single_Attribute
        (Attr_Id : GPR2.Q_Attribute_Id) return String
      is
         Attr : constant GPR2.Project.Attribute.Object :=
           Proj.Attribute (Attr_Id);
      begin
         if Attr.Kind /= Single then
            Error
              (String (Proj.Path_Name.Simple_Name)
               & ": "
               & Image (Attr_Id)
               & " value must be a single value");
            raise Parameter_Error;
         end if;

         return Attr.Value.Text;
      end Load_Single_Attribute;

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
               else Gnatcheck_Prj.Get_Project_Relative_File (Path.all));
         end loop;
      end if;

      --  Process additional GNATcheck switches
      if Proj.Has_Attribute (Switches_Attr, Ada_Idx) then
         List_Val := Load_List_Attribute (Switches_Attr, Indexed => True);
         Scan_Arguments (My_Project => My_Project, Args => List_Val);
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
        or else File_List_Specified
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
         Error (Ada.Exceptions.Exception_Message (E));
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
      if Gnatcheck.Options.Gnatcheck_Prj.Is_Specified
        and then Gnatcheck.Options.Gnatcheck_Prj.Tree.Is_Defined
      then
         return
           Normalize_Pathname
             (GNAT.Directory_Operations.Dir_Name
                (Gnatcheck_Prj.Tree.Root_Project.Path_Name.String_Value)
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
      use Gnatcheck.Projects.Aggregate;

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
            Error ("no runtime information found");
         end if;

         Error ("""" & Get_Aggregated_Project & """ processing failed");

         raise Parameter_Error;
      end if;

      if not My_Project.Tree.Update_Sources (GPR2.Sources_Units) then
         raise Parameter_Error;
      end if;

   exception
      when E : GPR2.Options.Usage_Error =>
         Error ("usage error: " & Ada.Exceptions.Exception_Message (E));
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

      if not My_Project.Tree.Load
               (Project_Options,
                Reporter         => Gpr2_Reporter,
                Absent_Dir_Error => GPR2.No_Error,
                With_Runtime     => True)
      then
         raise Parameter_Error;
      end if;

      if not My_Project.Tree.Languages.Contains (GPR2.Ada_Language) then
         Error
           (""""
            & String (My_Project.Tree.Root_Project.Path_Name.Simple_Name)
            & """ has no Ada sources, processing failed");

         raise Parameter_Error;

      elsif not My_Project.Tree.Has_Runtime_Project then
         --  Issue with the configuration of Ada
         for Msg of My_Project.Tree.Configuration.Log_Messages loop
            Print (Msg.Format);
         end loop;
         Error
           (""""
            & String (My_Project.Tree.Root_Project.Path_Name.Simple_Name)
            & """ processing failed");

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

            Gnatcheck.Projects.Aggregate.Collect_Aggregated_Projects
              (My_Project.Tree);

         end if;

         if not My_Project.Tree.Update_Sources then
            raise Parameter_Error;
         end if;
      end if;

   exception
      when E : GPR2.Options.Usage_Error =>
         Error ("usage error: " & Ada.Exceptions.Exception_Message (E));
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

      if Text_Report_ON then
         Report ("");
         Report ("Processing aggregated project " & Aggregated_Prj_Name);
         Report ("Expected report file: " & Expected_Text_Out_File);
      end if;

      if XML_Report_ON then
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

   ------------------------
   -- Set_Default_Target --
   ------------------------

   procedure Set_Default_Target is
   begin
      if not Gnatkp_Mode and then Should_Use_Codepeer_Target then
         GPR2.KB.Set_Default_Target ("codepeer");
      end if;
   end Set_Default_Target;

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
          (if not Arg.No_Object_Dir.Get and then Gnatcheck_Prj.Is_Specified
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

   procedure Store_Main_Unit (Unit_Name : String; Store : Boolean := True) is
   begin
      if Store then
         Gnatcheck.Projects.Main_Unit.Include (GPR2.Filename_Type (Unit_Name));
      end if;
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
         XML_Report ("<aggregated-project-reports>", Indent_Level => 1);
      end if;
   end Aggregate_Project_Report_Header;

   ------------------------------------
   -- Close_Aggregate_Project_Report --
   ------------------------------------

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type) is
   begin
      if XML_Report_ON then
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
      if Text_Report_ON then
         Report
           ("Exit code is"
            & Exit_Code'Img
            & " ("
            & (case Exit_Code is
                 when 0 => "no rule violation detected",
                 when 1 => "rule violation(s) detected",
                 when 2 => "tool failure, results cannot be trusted",
                 when 3 => "no rule check performed",
                 when others => "unknown")
            & ")");
      end if;

      if XML_Report_ON then
         XML_Report
           ("<exit-code>" & Image (Exit_Code) & "</exit-code>",
            Indent_Level => 3);

         XML_Report ("</aggregated-project>", Indent_Level => 2);
      end if;
   end Report_Aggregated_Project_Exit_Code;

   --------------------------
   -- Process_Rule_Options --
   --------------------------

   type Option_Kind is (File, Legacy_Option);

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
      --  First of all, process the provided LKQL rule file
      if LKQL_Rule_File_Name /= Null_Unbounded_String then
         Process_LKQL_Rule_File (To_String (LKQL_Rule_File_Name));
      end if;

      --  Then process the legacy rule options
      for O of Rule_Options loop
         case O.Kind is
            when File =>
               Process_Rule_File (To_String (O.Value));

            when Legacy_Option =>
               Process_Legacy_Rule_Option
                 (To_String (O.Value), Defined_At => "");
         end case;
      end loop;
      Process_Compiler_Instances;
   end Process_Rule_Options;

   ----------------------------
   -- Add_Legacy_Rule_Option --
   ----------------------------

   procedure Add_Legacy_Rule_Option (Opt : String; Prepend : Boolean := False) is
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
      Lower_Rule : constant String := To_Lower (Rule_Name);
      Prefix     : constant String :=
        (if Lower_Rule = "all" then "+" else "+R");
   begin
      Add_Legacy_Rule_Option (Prefix & Lower_Rule, Prepend => Prepend);
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
            then Gnatcheck_Prj.Get_Project_Relative_File (File)
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

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (My_Project : in out Arg_Project_Type;
      First_Pass : Boolean := False;
      Args       : GNAT.OS_Lib.Argument_List_Access := null)
   is

      Unknown_Opt_Parse_Args : XString_Vector;
      Args_After_Opt_Parse   : Argument_List_Access;
      Parser                 : Opt_Parser;

      function To_Arg_List (Args : XString_Vector) return Argument_List_Access;
      function To_XString_Array
        (Args : Argument_List_Access) return XString_Array;

      function To_Arg_List (Args : XString_Vector) return Argument_List_Access
      is
         Ret : constant Argument_List_Access :=
           new String_List (1 .. Args.Last_Index);
      begin
         for I in Ret'Range loop
            Ret (I) := new String'(Args (I).To_String);
         end loop;
         return Ret;
      end To_Arg_List;

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
                    (Option_Record'
                       (File,
                        To_Unbounded_String (Parameter (Parser => Parser))));
                  if not More_Then_One_Rule_File_Set then
                     Rule_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     More_Then_One_Rule_File_Set := True;
                  else
                     Free (Rule_File_Name);
                  end if;

               when others =>
                  Add_Legacy_Rule_Option (Full_Switch (Parser => Parser));
                  Individual_Rules_Set := True;
            end case;
            if not Rules_Depreciation_Emitted then
               Info_In_Tty
                 ("The '-rules' section is now deprecated. You should only use"
                  & " the '--rule' and '--rule-file' command-line options.");
               Info_In_Tty
                 ("You can use the '--emit-lkql-rule-file' flag to "
                  & "automatically translate your rule configuration to the "
                  & "new LKQL format.");
               Rules_Depreciation_Emitted := True;
            end if;
         end loop;
      end Process_Sections;

      Executable : String_Access :=
        GNAT.OS_Lib.Locate_Exec_On_Path (Ada.Command_Line.Command_Name);
      Prefix     : constant String :=
        Containing_Directory (Containing_Directory (Executable.all));
      Lkql       : constant String :=
        Compose (Compose (Prefix, "share"), "lkql");

      Args_From_Project : constant Boolean := Args /= null;
      Initial_Char      : Character;
      Success           : Boolean;

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
            Disallow (Arg.Follow_Symbolic_Links.This, "-eL" & In_Project_Msg);
            Disallow (Arg.Lkql_Path.This, "--lkql-path" & In_Project_Msg);
            Disallow (Arg.Rules.This, "-r" & In_Project_Msg);
            Disallow (Arg.Rule_File.This, "--rule-file" & In_Project_Msg);
            Disallow (Arg.Target.This, "--target" & In_Project_Msg);
            Disallow (Arg.RTS.This, "--RTS" & In_Project_Msg);
         end;
      end if;

      if Arg.Parser.Parse
           ((if Args /= null then To_XString_Array (Args) else No_Arguments),
            Unknown_Arguments => Unknown_Opt_Parse_Args)
      then
         Args_After_Opt_Parse := To_Arg_List (Unknown_Opt_Parse_Args);
         Initialize_Option_Scan
           (Parser, Args_After_Opt_Parse, Section_Delimiters => "cargs rules");
      else
         raise Parameter_Error;
      end if;

      --  Reallow arguments that were disallowed
      if Args_From_Project then
         Allow (Arg.Transitive_Closure.This);
         Allow (Arg.Scenario_Vars.This);
         Allow (Arg.Aggregate_Subproject.This);
         Allow (Arg.Project_File.This);
         Allow (Arg.Follow_Symbolic_Links.This);
         Allow (Arg.Lkql_Path.This);
         Allow (Arg.Rules.This);
         Allow (Arg.Rule_File.This);
         Allow (Arg.Target.This);
         Allow (Arg.RTS.This);
      end if;

      loop
         Initial_Char :=
           GNAT.Command_Line.Getopt
             ("v h hx "
              & "m? files= a "
              & "vP! "
              &   --  project-specific options
                                               "-kp-version= "
              & "o= "
              & "ox= "
              & "log "
              & "-subprocess "
              & "-version -help "
              & "nt xml",
              Parser => Parser);

         case Initial_Char is
            when ASCII.NUL =>
               Success := False;

               loop
                  declare
                     Arg : constant String :=
                       Get_Argument (Do_Expansion => True, Parser => Parser);

                  begin
                     exit when Arg = "";
                     Success := True;

                     if Options.Arg.Transitive_Closure.Get then
                        Gnatcheck.Projects.Store_Main_Unit
                          (Arg, Args_From_Project or First_Pass);
                     else
                        Store_Sources_To_Process
                          (Arg, Args_From_Project or First_Pass);

                        if not Args_From_Project then
                           Argument_File_Specified := True;
                        end if;
                     end if;
                  end;
               end loop;

               exit when not Success;

            when 'a' =>
               --  Ignore -a for compatibility

               null;

            when 'f' =>
               if Full_Switch (Parser => Parser) = "files" then
                  File_List_Specified := True;

                  if First_Pass then
                     Files_Switch_Used := True;
                     Read_Args_From_File (Parameter (Parser => Parser));

                  elsif Args_From_Project then
                     Read_Args_From_File (Parameter (Parser => Parser));
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

            when 'l' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "log" then
                     Log_Mode := True;
                  end if;
               end if;

            when 'm' =>
               if not First_Pass then
                  begin
                     Max_Diagnoses :=
                       Natural'Value (Parameter (Parser => Parser));

                     if Max_Diagnoses > 1000 then
                        Error
                          ("Parameter (Parser => Parser) of '-m' option "
                           & "too big, max allowed is 1000");
                        raise Parameter_Error;
                     end if;

                  exception
                     when Constraint_Error =>
                        Error
                          ("Wrong Parameter of '-m' option: "
                           & Parameter (Parser => Parser));
                        raise Parameter_Error;
                  end;
               end if;

            when 'n' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "nt" then
                     Text_Report_ON := False;
                     XML_Report_ON := True;
                  end if;
               end if;

            when 'o' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     Set_Report_File_Name (Parameter (Parser => Parser));
                     Custom_Text_Report_File := True;

                  elsif Full_Switch (Parser => Parser) = "ox" then
                     Set_XML_Report_File_Name (Parameter (Parser => Parser));
                     XML_Report_ON := True;
                     Custom_XML_Report_File := True;
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
                           Error
                             ("wrong switch parameter "
                              & Parameter
                              & " for -vP");
                           raise Parameter_Error;
                     end;
                  elsif Args_From_Project then
                     Error ("-vP option is not allowed in a project file");
                     raise Parameter_Error;
                  end if;
               end if;

            when 'x' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "xml" then
                     XML_Report_ON := True;
                  end if;
               end if;

            when '-' =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "-kp-version" then
                     Free (KP_Version);
                     KP_Version := new String'(Parameter (Parser => Parser));
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-help" then
                     if Args_From_Project then
                        Error
                          ("project file should not contain '--help' option");
                        raise Parameter_Error;
                     end if;

                     Print_Usage := True;
                     return;

                  elsif Full_Switch (Parser => Parser) = "-version" then
                     if Args_From_Project then
                        Error
                          ("project file should not contain '--version' "
                           & "option");
                        raise Parameter_Error;
                     end if;

                     Print_Version := True;

                  end if;
               end if;

            when others =>
               Error
                 ("unrecognized switch: " & Full_Switch (Parser => Parser));
               raise Parameter_Error;
         end case;
      end loop;

      if Current_Section (Parser => Parser) = "" and then not First_Pass then
         Process_Sections;
      end if;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Error ("invalid switch: " & Full_Switch (Parser => Parser));
         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Error
           ("missing Parameter (Parser => Parser) for: -"
            & Full_Switch (Parser => Parser));
         raise Parameter_Error;
   end Scan_Arguments;

   ---------------------------
   -- Store_Compiler_Option --
   ---------------------------

   package Compiler_Switches is new
     GNAT.Table
       (Table_Component_Type => String_Access,
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
      use Ada.Strings.Unbounded;
   begin
      if Verbose_Mode and then not Arg.Aggregated_Project then
         --  When procressing aggregated projects one by one, we want
         --  Verbose_Mode to print this only in the outer invocation.
         Print_Version_Info (2004);
      end if;

      --  We generate the rule help unconditionally

      if Generate_Rules_Help and then not Arg.Aggregated_Project then
         Rules_Help;
      end if;

      if Gnatcheck.Options.Generate_XML_Help
        and then not Arg.Aggregated_Project
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

      if Generate_Rules_Help or else Generate_XML_Help then
         Nothing_To_Do := True;
         return;
      end if;

      Read_Args_From_Temp_Storage
        (Duplication_Report => not Is_Specified (Gnatcheck_Prj),
         Arg_Project        => Gnatcheck_Prj);
      Nothing_To_Do := Last_Source < First_SF_Id;

      if Nothing_To_Do then
         Error ("no existing file to process");
         return;
      end if;

      Gnatcheck.Projects.Set_Global_Result_Dirs (Gnatcheck_Prj);
      Gnatcheck_Config_File :=
        new String'(Global_Report_Dir.all & Gnatcheck_Config_File.all);

      Analyze_Compiler_Output :=
        Use_gnaty_Option
        or Use_gnatw_Option
        or Check_Restrictions
        or Arg.Check_Semantic.Get;

      if Analyze_Compiler_Output then
         Store_Compiler_Option ("-gnatec=" & Gnatcheck_Config_File.all);
         Store_Compiler_Option ("-gnatcU");
         Store_Compiler_Option ("-gnatwnA.d");

         if Use_gnatw_Option then
            Store_Compiler_Option (Get_Warning_Option);
         end if;

         Store_Compiler_Option ("-gnatyN");

         if Use_gnaty_Option then
            for S of Create (Get_Style_Option, " ") loop
               Store_Compiler_Option (S);
            end loop;
         end if;
      end if;

      --  If GNATcheck is in KP mode and there is a command line specified KP
      --  version, we have to iterate over all implemented rules to enable
      --  those which match the version.
      if Gnatkp_Mode and then KP_Version /= null then
         for Rule_Cursor in All_Rules.Iterate loop
            declare
               Id       : constant Rule_Id := Rule_Map.Key (Rule_Cursor);
               Rule     : constant Rule_Info := All_Rules (Rule_Cursor);
               Instance : Rule_Instance_Access;
            begin
               if Rule.Impact /= null
                 and then Match (KP_Version.all, Rule.Impact.all)
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
         if Gnatkp_Mode and then KP_Version /= null then
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

      --  Save compiler switches computed in Compiler_Arg_List

      Compiler_Arg_List :=
        new Argument_List'
          (String_List
             (Compiler_Switches.Table (1 .. Compiler_Switches.Last)));

      <<Processing_Aggregate_Project>>

      Ada.Directories.Create_Path (Global_Report_Dir.all);
      Gnatcheck.Output.Set_Report_Files;
   end Check_Parameters;

end Gnatcheck.Projects;
