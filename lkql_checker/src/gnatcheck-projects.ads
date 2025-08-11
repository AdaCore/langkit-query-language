--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package contains high-level interfaces to project files for gnatcheck.
--
--  The interface provided here assumes that a tool is based on the Source
--  Table defined in Gnatcheck.Source_Table.
--
--  The recommended way of project file processing is:
--
--  Two main steps of processing the project file that is the tool argument
--  should be done:
--
--  (a) analyzing the project file as a part of the analysis of the tool
--      command line arguments. At this stage the following information is
--      supposed to be extracted and stored in the tool data structures:
--
--      * options to be used to call gcc to create tree files;
--      * tool-specific options;
--      * a list of argument sources (in case when a tool is called without
--        argument sources, that means that all the sources of the project
--        should be processed);
--
--  (b) creating the mapping file for the project sources and storing the
--      compilation options that are specific for the argument sources.
--
--  The general tool architecture defines the following sequence of tool
--  initialization:
--
--  (1) tool parameters are analyzed and stored, at this stage the tool
--      argument files are stored in some temporary storage, but not in the
--      source table;
--
--  (2) a check is made that the set of tool options is consistent, and if
--      it is, the argument sources are moved from the temporary storage into
--      the source table, and at this stage it is checked that a source to be
--      placed in the source table exists, and that there is no duplication
--      in the set of argument sources. As a result, the source table
--      contains only existing sources with no duplication.
--
--  The step (a) above is a part of the step (1) of the general tool
--  initialization, but the step (b) can be done only when we have a list of
--  the existing argument files with no duplications, so it is performed as a
--  part of step (2) *after* moving the argument files into the source table.
--  This is why there are basically two places in the tool code that is
--  responsible for the project file processing.
--
--  *** Processing tool parameters ***
--
--  We have to process tool command line parameters twice and in between we
--  have to process the project file. The reason is that tool options may be
--  specified both on the command line and in the tool-specific package of the
--  project file, and the resulting list of options to be applied to the tool
--  should be the superposition of the options from the project file and from
--  the command line in the following order:
--
--   <options from project file><options from command line>
--
--  So, the processing of the tool parameters should be a sequence of three
--  steps:
--
--  1. Scan the command-line parameters, but store only the following
--     information:
--     * project-specific options (-P, -U, -X, -vP)
--     * '--help' and '--version'
--     * argument sources (specified either directly or by any other means,
--       for example as '--files=list_of_arg_sources.txt'
--       where list_of_arg_sources.txt is a text file containing the names of
--       the argument sources
--
--  2. If at the first step either '--help' or '--version' option is detected,
--     print out the corresponding information and exit from the tool. Else if
--     at the first step '-P prj' option is detected, analyze the project file
--     prj (taking into account all the other project-specific options). This
--     includes extracting the tool options, compilation parameters for
--     creating tree files and argument sources (if needed).
--
--  3. Scan again the command-line parameters, but this time ignore all the
--     options listed in paragraph 1 above, but store all the other options.
--     This gives the required superposition of the options specified in the
--     project file and in the command line.
--
--  The same scanner is used to process the command line and the
--  values of the Default_Switches attribute.
--  This procedure with the appropriate command line parser and
--  boolean parameters that indicate the case of ins usage) implements steps 1
--  and 3 above. For step 2, see the procedure Process_Project_File
--  that combines all the steps of loading and analyzing the project file.

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GPR2.Containers;
with GPR2.Project.Tree;
with GPR2.Project.View;

package Gnatcheck.Projects is

   ------------------------------
   -- Project-specific options --
   ------------------------------

   --------------------------------------------------------------------------
   -- -U [main_unit]  : get the source or main_unit closure of the project --
   --------------------------------------------------------------------------

   File_List_Specified : Boolean := False;
   --  Indicates if '-files=...' option is specified. We need to know this when
   --  getting the list of files from the project - '-U' should be  ignored if
   --  '-files=...' is specified.

   Main_Unit : GPR2.Containers.Filename_Set;
   --  If the tool is called with "... Pproj -U main_unit1 main_unit2 ...",
   --  main units are stored here.

   procedure Store_Main_Unit (Unit_Name : String; Store : Boolean := True);
   --  Processes the result returned by GNAT.Comand_Line.Get_Argument provided
   --  that it is supposed to be the main unit name for '-U' project file
   --  option.
   --
   --  If Store is ON, stores the name of the unit. If Store is OFF,
   --  does not store anything.

   -----------------------------------------------------------
   --  -Xvariable=value  : set values of external variables --
   -----------------------------------------------------------

   --  An external variable is the parameter of a '-X<par>' tool option, it
   --  is guaranteed that it has a structure <variable_name>=<value>

   procedure Store_External_Variable (Var : String);
   --  Checks if the value for the given external variable is already stored
   --  (the check is case-sensitive), and if it is not, stores the value
   --  setting for the given variable. If it is, overrides the stored value.

   procedure Append_Variables
     (Args : in out Argument_List; Last : in out Natural);
   --  Append a "-XVAR=value" string for each stored external variable

   function Subdir_Name return String;
   --  Return the subdir name to use, if one was set explicitly.

   ----------------------------------------------------------------
   -- --print-gpr-registry : print gnatcheck attributes and exit --
   ----------------------------------------------------------------

   Print_Gpr_Registry : Boolean := False;
   --  If this flag is ON, gpr attributes registered by gnatcheck are printed
   --  and gnatcheck exit returning 0.

   ------------------
   -- Rule options --
   ------------------

   type Option_Kind is (File, Legacy_Option, Single_Rule_Name);

   type Option_Record is record
      Kind  : Option_Kind;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Vector_Options is new
     Ada.Containers.Vectors (Positive, Option_Record);

   Rule_Options : Vector_Options.Vector;

   ---------------------------------------------------------
   -- Type to represent a project passed as a tool option --
   ---------------------------------------------------------

   type Arg_Project_Type is tagged private;
   --  This type is the base for each tool-specific project type. Most of its
   --  primitives does not need any redefinition for a specific tool.

   function Tree
     (My_Project : Arg_Project_Type) return GPR2.Project.Tree.Object;
   --  Returns access to project tree object

   function View
     (My_Project : Arg_Project_Type) return GPR2.Project.View.Object;
   --  Returns access to project view object

   procedure Error (My_Project : Arg_Project_Type; Message : String);
   --  Emit an error message about this ``My_Project`` project

   procedure Store_Project_Source
     (My_Project : in out Arg_Project_Type; Project_File_Name : String);
   --  If Project_File_Name ends with ".gpr", it is taken to be the name of
   --  the project file; otherwise Project_File_Name & ".gpr" is used.
   --  Checks that:
   --    - this is the first -P option provided as a tool parameter;
   --    - the project file exists.
   --  Raises Gnatcheck.Common.Parameter_Error if any of these check fails,
   --  stores the name of the project file My_Project otherwise.

   procedure Store_CGPR_Source
     (My_Project : in out Arg_Project_Type; CGPR_File_Name : String);
   --  Stores configuration project file.
   --  Checks that:
   --    - this is the first --config option provided as a tool parameter;
   --    - the configuration project file exists.???
   --  Raises Gnatcheck.Common.Parameter_Error if any of these check fails,
   --  stores the name of the configuration project file otherwise.

   procedure Store_Compiler_Option (Switch : String);
   --  Stores a compiler option as is.

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean;
   --  Checks if the argument represents a project that corresponds to some
   --  project file specified as a tool parameter.

   function Get_Project_Relative_File
     (My_Project : Arg_Project_Type; Filename : String) return String;
   --  From the given ``Filename``, get the absolute path leading to it
   --  realtively to the current project file. If there is no specified
   --  project file, then get the file from the current directory.

   procedure Clean_Up (My_Project : Arg_Project_Type);
   --  Removes all the temporary files created when loading a project. Does
   --  nothing of Debug_Flag_N is ON.

   function Source_Prj (My_Project : Arg_Project_Type) return String;
   --  If My_Project.Is_Specified then returns the full normalized name of the
   --  project file, otherwise returns a null string.

   function Source_CGPR (My_Project : Arg_Project_Type) return String;
   --  If My_Project.Source_CGPR is specified then returns its value,
   --  otherwise returns a null string.

   procedure Set_External_Values (My_Project : Arg_Project_Type);
   --  For each value of an external variable that has been stored as a result
   --  of the initial parameter processing, changes environment accordingly.
   --  Any inconsistencies coming from improper values of scenario variables
   --  etc. will be reported during project loading.

   procedure Get_Sources_From_Project (My_Project : in out Arg_Project_Type);
   --  Extracts and stores the list of sources of the project to process as
   --  tool arguments.
   --
   --  Currently, main units are ignored pending libgpr2 support for computing
   --  a closure on main units, so basically -U is implicit.

   procedure Set_Global_Result_Dirs (My_Project : in out Arg_Project_Type);
   --  Sets the directory to place the global tool results into.

   procedure Register_Tool_Attributes (My_Project : Arg_Project_Type);
   --  Register tool specific attributes. In particular, gnatcheck needs
   --  to recognise Codepeer.File_Patterns.

   procedure Extract_Tool_Options (My_Project : in out Arg_Project_Type);
   --  Extracts gnatcheck options from the project file

   procedure Process_Rule_Options;
   --  Process all the rule options found as part of scanning arguments

   procedure Add_Legacy_Rule_Option (Opt : String; Prepend : Boolean := False);
   --  Add the given ``Opt`` to the list of rule options processed by
   --  ``Process_Rule_Options`` as a command-line rule option (e.g. +R...).
   --  If ``Prepend`` is set to True, add the rule option at the start of
   --  the processing list.

   procedure Add_Rule_By_Name (Rule_Name : String; Prepend : Boolean := False);
   --  Use ``Add_Legacy_Rule_Option`` to forge a new rule option enabling the
   --  given rule without any parameter.

   procedure Set_LKQL_Rule_File (File : String; Project_Relative : Boolean);
   --  Set the given ``File`` as the LKQL rule file to process during the
   --  execution of ``Process_Rule_Options``. If a rule file has already been
   --  set, this function displays an error and set the
   --  ``Rule_Option_Problem_Detected`` flag to True.
   --  If the provided ``File`` isn't an absolute path, if ``Project_Relative``
   --  is set to ``True``, resolve the provided file relatively to
   --  the current project file (if any). Else, resolve ``File`` relatively to
   --  the current working directory.

   function Is_Rule_Options_Empty return Boolean;
   --  Get whether the rule options are empty.

   procedure Aggregate_Project_Report_Header (My_Project : Arg_Project_Type);
   --  Prints header in the summary report file created if the argument project
   --  is an aggregate project. In this case a tool is spawned to run separatly
   --  for each project being aggregated, and each such run creates its own
   --  report separate file.

   procedure Close_Aggregate_Project_Report (My_Project : Arg_Project_Type);
   --  Finalizes in the summary report file created if the argument project
   --  is an aggregate project. In this case a tool is spawned to run separatly
   --  for each project being aggregated, and each such run creates its own
   --  report separate file.
   --
   --  The default version of this procedure does nothing except closing the
   --  aggregated-project-reports tag in summary XML report file (if XML report
   --  mode is ON).

   procedure Report_Aggregated_Project
     (Aggregate_Prj          : Arg_Project_Type;
      Aggregated_Prj_Name    : String;
      Expected_Text_Out_File : String;
      Expected_XML_Out_File  : String);
   --  Starts a record about processing of an aggregated project in a summary
   --  report file if the tool argument project is an aggregate project. By
   --  default, prints out the name of the aggregated project to process and
   --  the name(s) of the report file(s) that is (are) expected to be created.

   procedure Report_Aggregated_Project_Exit_Code
     (Aggregate_Prj : Arg_Project_Type; Exit_Code : Integer);
   --  Starts a record about processing of an aggregated project in a summary
   --  report file if the tool argument project is an aggregate project. By
   --  default prints out the (text image of the) exit code.

   -------------------------------------
   -- General project file processing --
   -------------------------------------

   procedure Initialize_Environment;
   --  Initializes the environment for extracting the information from the
   --  project file. This includes setting the parameters specific for the
   --  given tool version assuming that the tools for cross environment are
   --  named in a standard way (that is, <cross-prefix>-<tool_name>.

   procedure Process_Project_File (My_Project : in out Arg_Project_Type'Class);
   --  Combines all the actions needed to process the argument project file
   --  except storing individual compilation options for argument files.

   -------------------------------------
   -- General command line processing --
   -------------------------------------

   procedure Check_Parameters;
   --  Checks that the parameters are compatible with each other

private

   type Arg_Project_Type is tagged record
      Tree        : aliased GPR2.Project.Tree.Object;
      View        : aliased GPR2.Project.View.Object;
      Source_Prj  : String_Access;
      Source_CGPR : String_Access;
   end record;

   function Tree
     (My_Project : Arg_Project_Type) return GPR2.Project.Tree.Object
   is (My_Project.Tree);

   function View
     (My_Project : Arg_Project_Type) return GPR2.Project.View.Object
   is (My_Project.View);

end Gnatcheck.Projects;
