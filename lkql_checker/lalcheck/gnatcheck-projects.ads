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

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with GNATCOLL.VFS;      use GNATCOLL.VFS;

with GPR2.Path_Name.Set;
with GPR2.Project.Tree;
with GPR2.Project.Source;

package Gnatcheck.Projects is

   ------------------------------
   -- Project-specific options --
   ------------------------------

   ---------------------------------------------------------------------
   -- -eL  : Follow all symbolic links when processing project files. --
   ---------------------------------------------------------------------

   Follow_Symbolic_Links : Boolean := False;

   ------------------------------------------------------
   -- -vPn  : verbosity level on project file analysis --
   ------------------------------------------------------

   subtype Verbosity_Levels is Natural range 0 .. 2;

   Verbosity_Level : Verbosity_Levels := 0;

   --------------------------------------------------------------------------
   -- -U [main_unit]  : get the source or main_unit closure of the project --
   --------------------------------------------------------------------------

   File_List_Specified : Boolean := False;
   --  Indicates if '-files=...' option is specified. We need to know this when
   --  getting the list of files from the project - '-U' should be  ignored if
   --  '-files=...' is specified.

   Recursive_Sources : Boolean := True;
   --  Indicates that all sources of all projects should be processed
   --  as opposed to sources of the root project only.

   U_Option_Set : Boolean := False;
   --  Indicates if -U option is specified for the project (to process all the
   --  units of the closure of the argument project or to process the closure
   --  of the main unit if the main unit is set)

   Main_Unit : GPR2.Path_Name.Set.Object;
   --  If the tool is called with "... Pproj -U main_unit1 main_unit2 ...",
   --  main units are stored here.

   procedure Store_Main_Unit
     (Unit_Name : String;
      Store     : Boolean := True);
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
     (Args : in out Argument_List;
      Last : in out Natural);
   --  Append a "-XVAR=value" string for each stored external variable

   --------------------------------------------------------------------------
   -- --subdirs=<d>  : <d> is subdirectories to place the tool output into --
   --------------------------------------------------------------------------

   Subdir_Name : String_Access;
   --  If Subdir_Name is null, no special subdirectory is used for tool
   --  results.

   procedure Set_Subdir_Name (S : String);
   --  Sets Subdir_Name to S (if Subdir_Name is not null, frees the old value).
   --  We may have '--subdirs=<d>' option both in command line and in the list
   --  of values of Default_Switches and Switches attributes in the tool
   --  package in the project file.

   ----------------------------------------------------------------
   -- --no_object_dir : do not place the results into object dir --
   ----------------------------------------------------------------

   No_Object_Dir : Boolean := False;
   --  If this flag is ON, the output files are placed in the current directory

   ---------------------------------------------------------
   -- Type to represent a project passed as a tool option --
   ---------------------------------------------------------

   type Arg_Project_Type is tagged limited private;
   --  This type is the base for each tool-specific project type. Most of its
   --  primitives does not need any redefinition for a specific tool.

   function Tree
     (My_Project : Arg_Project_Type) return access GPR2.Project.Tree.Object;
   --  Returns access to project tree object

   procedure Store_Project_Source
     (My_Project        : in out Arg_Project_Type;
      Project_File_Name : String);
   --  If Project_File_Name ends with ".gpr", it is taken to be the name of
   --  the project file; otherwise Project_File_Name & ".gpr" is used.
   --  Checks that:
   --    - this is the first -P option provided as a tool parameter;
   --    - the project file exists.
   --  Raises Gnatcheck.Common.Parameter_Error if any of these check fails,
   --  stores the name of the project file My_Project otherwise.

   function Is_Specified (My_Project : Arg_Project_Type) return Boolean;
   --  Checks if the argument represents a project that corresponds to some
   --  project file specified as a tool parameter.

   function Files (My_Project : Arg_Project_Type) return File_Array_Access;
   --  Return the files associated with My_Project if any, null otherwise

   procedure Clean_Up (My_Project : Arg_Project_Type);
   --  Removes all the temporary files created when loading a project. Does
   --  nothing of Debug_Flag_N is ON.

   function Source_Prj (My_Project : Arg_Project_Type) return String;
   --  If My_Project.Is_Specified then returns the full normalized name of the
   --  project file, otherwise returns a null string.

   procedure Load_Tool_Project (My_Project : in out Arg_Project_Type);
   --  Loads argument project

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

   procedure Scan_Arguments
     (My_Project  : in out Arg_Project_Type;
      First_Pass  : Boolean    := False;
      Parser      : Opt_Parser := Command_Line_Parser;
      In_Switches : Boolean    := False);
   --  This procedure should be redefined for each tool project type. It
   --  should be called immediately after the call to Initialize_Option_Scan
   --  that should create the Parser for it. The procedure defines the loop
   --  through the parameters - either command-line parameters or tool
   --  parameters defined in a tool-specific package of the tool argument
   --  project file.
   --  If the actual for Parser is different from Command_Line_Parser, the
   --  procedure assumes the options from the project file. If In_Switches is
   --  ON, it assumes that the options are the values of the Switches
   --  attribute, otherwise - of the Default_Switches attribute.
   --  This procedure is supposed to be used for processing command-line
   --  parameters twice - first, it detects and stores only project-specific
   --  attributes, argument sources and '--help' and "--version' options, when
   --  called for this, it should have First_Pass parameter ON. Then, after
   --  processing the project file (if it is provided as a tool parameter and
   --  detected during the first pass through the command-line parameters),
   --  all the other command-line parameters should be processed and stored
   --  (this allows the parameters specified in the command line to override
   --  the parameters given in the project file), and when called for this,
   --  the procedure should have First_Pass set OFF.

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
     (Aggregate_Prj : Arg_Project_Type;
      Exit_Code     : Integer);
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

   type Arg_Project_Type is tagged limited record
      Tree       : aliased GPR2.Project.Tree.Object;
      Source_Prj : String_Access;

      Files : File_Array_Access;
      --  Files associated with this project, when using --simple-project
   end record;

   function Tree
     (My_Project : Arg_Project_Type)
      return access GPR2.Project.Tree.Object is (My_Project.Tree.Reference);

end Gnatcheck.Projects;
