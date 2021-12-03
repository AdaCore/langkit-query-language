------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2004-2021, AdaCore                     --
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

--  This package defines the source file table - the table containing the
--  information about the source files to be processed and the state of their
--  processing. Used by Several_Files_Driver.

with Ada.Containers.Indefinite_Ordered_Sets;

with GNATCOLL.Projects;    use GNATCOLL.Projects;

with Gnatcheck.Projects;   use Gnatcheck.Projects;
with Checker_App;          use Checker_App;

package Gnatcheck.Source_Table is

   Low_SF_Bound  : constant := 0;
   High_SF_Bound : constant := 999_999;
   --  Almost 1_000_000 source files for one run of the tool

   type SF_Id is range Low_SF_Bound .. High_SF_Bound;

   No_SF_Id    : constant SF_Id := Low_SF_Bound;
   First_SF_Id : constant SF_Id := No_SF_Id + 1;

   Total_Sources : Natural;
   Sources_Left  : Natural;
   --  Counters used to form and output progress information.

   type SF_Status is (
      Waiting,
      --  Waiting for processing

      Not_A_Legal_Source,
      --  The file does not contain compilable source

      Error_Detected,
      --  Some tool problem has been detected when processing this source
      --  so the results of processing may not be safe

      Processed
      --  The source file has been successfully processed
      );

   type SF_Info is new Integer;
   --  The type to be used for the integer values associate with each source in
   --  the source file table. The use of this value is client-specific

   function Present (SF : SF_Id) return Boolean;
   --  Checks that SF is not is equal to No_SF_Id

   function Get_File_Names_Case_Sensitive return Integer;
   pragma Import (C, Get_File_Names_Case_Sensitive,
                  "__gnat_get_file_names_case_sensitive");
   File_Names_Case_Sensitive : constant Boolean :=
                                 Get_File_Names_Case_Sensitive /= 0;
   --  Set to indicate whether the operating system convention is for file
   --  names to be case sensitive (e.g., in Unix, set True), or non case
   --  sensitive (e.g., in Windows, set False). This code is taken as is from
   --  the GNAT Osint package to avoid dependency on Osint

   function File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False;
      Case_Sensitive : Boolean := File_Names_Case_Sensitive)
      return           SF_Id;
   --  Returns the Id of the file with name SF_Name stored in the files
   --  table. Returns No_SF_Id if the table does not contain such a file.
   --  if Use_Short_Name parameter is True, the short file name is used to
   --  locate the file; if the argument contains a directory information it is
   --  stripped out. Otherwise this function tries to locate the name with the
   --  full normalized name equal to SF_Name.
   --  If Case_Sensitive is False, then this function first looks for the
   --  SF_Name using the original casing of SF_Name and files stored in the
   --  Source Table, and if it cannot locate the file, it repeats the search
   --  with all the path/file names converted to lower case.

   procedure Adjust_From_Source_Table (S : in out String);
   --  Assuming that S is a name of some source file, tries to locate this
   --  source file in the source table. If the attempt is successful and the
   --  length of the stored source name is the same as S'Length, replaces
   --  S with the stored source name. This procedure is needed in Windows
   --  if we get some source name from a compiler message (and the compiler
   --  always folds file names to lower case in Windows), but we are interested
   --  the original casing of the name.

   procedure Store_Sources_To_Process
     (Fname : String;
      Store : Boolean := True);
   --  Fname is stored in an internal database as the name of the file to be
   --  processed by the tool. No check is made if Fname denotes an existing
   --  file.
   --
   --  If Store is OFF then the procedure does not store anything.
   --
   --  If Fname is not an empty string, sets the
   --  Gnatcheck.Options.No_Argument_File_Specified flag OFF

   procedure Read_Args_From_Temp_Storage
     (Duplication_Report : Boolean;
      Arg_Project        : Arg_Project_Type'Class;
      Status             : SF_Status := Waiting);
   --  Reads argument files from temporary storage (where they are placed by
   --  Store_Sources_To_Process/Store_Args_From_File routine(s)). Uses
   --  Add_Source_To_Process to read each file, so the check if a file exists
   --  is performed on the base of the source search path
   --  (ASIS_UL.Compiler_Options.Source_Search_Path) or the project file that
   --  is a tool argument. This procedure calls Add_Source_To_Process for each
   --  file to do the existence test and to store source in the source table.
   --  The temporary storage is cleaned up.
   --
   --  The Duplication_Report parameter has the same meaning as for
   --  Add_Source_To_Process.
   --
   --  If the actual for Arg_Project denotes a project specified as a tool
   --  parameter then this procedure tries to store all the subunits *after*
   --  the units that are enclosing bodies for these subunits. This is needed
   --  to make sure that subunits will be processed on the base of the trees
   --  created for enclosing bodies, because only in this case the tree
   --  representing a subunit is attributed properly.

   procedure Read_Args_From_File (Par_File_Name : String);
   --  Reads argument files from the file. Stores the file names in the
   --  temporary storage as Store_Sources_To_Process does. This procedure
   --  assumes that the file named by Par_File_Name contains argument file
   --  names, one per line.
   --
   --  This procedure sets Gnatcheck.Options.No_Argument_File_Specified to
   --  False.

   function Files_In_Temp_Storage return Natural;
   --  Returns the number of files stored in temporary storage.

   function First_File_In_Temp_Storage return String;
   --  Returns the simple name of the first file in the temporary file storage
   --  (according to the way how the storage is ordered). A caller should make
   --  sure that the temporary storage is not empty.
   --
   --  What actually is needed on a tool side is the name of the file if this
   --  file is the only file that is kept in the temporary storage. So it may
   --  make sense to revise this function.

   procedure Temp_Storage_Iterate
     (Action : not null access procedure (File_Name : String));
   --  Call Action for each File_Name in the temporary file storage

   function Arg_Source_File_Name return String;
   --  If the tool is called with all the argument sources specified in a
   --  single text file (using '-files=arg_files' option), returns the name of
   --  this file. Otherwise returns null string.
   --  If the tool is called from the GNAT driver, the result of this function
   --  corresponds to the call generated by the GNAT driver, but not to the
   --  call to the GNAT driver.

   Individual_Files_Specified : Boolean := False;
   --  Flag indicating if for the tool call there is at least one source file
   --  explicitly specified in the command line. Note that the tool has itself
   --  to take care of proper setting of this flag.

   function Last_Source return SF_Id;
   --  Returns the Id of the last source stored in the source table. Returns
   --  No_SF_Id if there is no source file stored

   function Total_Sources_To_Process return Natural;
   --  Returns the number of the arument sources to be processed. This may be
   --  different from Last_Source if '--ignore=...' option specifies the list
   --  of files to be ignored.

   function Exempted_Sources return Natural;
   --  Returns the number of (existing) sources marked as ignored/exempted as
   --  the result of '--ignore=...' option.

   function Last_Argument_Source return SF_Id;
   --  Returns the Id of the last argument source stored in the source table.
   --  An argument source is the source set as the argument of the tool call.

   function Is_Argument_Source (SF : SF_Id) return Boolean;
   --  Checks if SF is from tool argument sources

   function Is_Needed_Source (SF : SF_Id) return Boolean;
   --  Checks if SF is a source that has been added as a needed source for some
   --  argument source (i.e. Is_Argument_Source is False).

   procedure Output_Source (SF : SF_Id);
   --  Depending on the options set generates the trace of the units/sources
   --  processing. If Verbose_Mode is ON, outputs into Stderr the number of the
   --  units left and the name of the source being processed. Otherwise, if
   --  Quiet_Mode is OFF, outputs only the number of units left. If
   --  Progress_Indicator_Mode is ON, generates the output to be used for GPS
   --  progress indicator. (Unconditionally) decreases the counter of the
   --  sources which have to be processed (Sources_Left)

   procedure Source_Clean_Up
     (SF             : SF_Id;
      Keep_ALI_Files : Boolean := False);
   --  Minimal clean-up needed for one source (closing and dissociating the
   --  Context, removing the tree and ALI files created for this source in
   --  the temporary directory, if Keep_ALI_Files is set ON, ALI file(s) is
   --  (are) not deleted).

   function Create_Context return LKQL_Context;
   --  Create the LKQL context

   procedure Add_Sources_To_Context
     (Ctx     : LKQL_Context;
      Project : Arg_Project_Type'Class);
   --  Add all sources from Project to Ctx

   procedure Process_Sources
     (Ctx : LKQL_Context; Annotate_Only : Boolean := False);
   --  Procedure all sources. Only process pragma Annotate if Annotate_Only
   --  is true.

   ----------------------------------------
   -- Source file access/update routines --
   ----------------------------------------

   function Source_Name (SF : SF_Id) return String;
   --  Returns the full source file name in absolute normalized form.

   function Short_Source_Name (SF : SF_Id) return String;
   --  Short file name with no directory information

   function Suffixless_Name (SF : SF_Id) return String;
   --  Returns the file name with no directory information and with
   --  no suffix (if any). Can be used to create the name of the tree and
   --  ALI file that correspond to SF.

   function CU_Name (SF : SF_Id) return String;
   procedure Set_CU_Name (SF : SF_Id; N : String);
   --  Returns (sets) the full expanded Ada name of the compilation unit that
   --  is contained in the source.

   function  Source_Status     (SF : SF_Id) return SF_Status;
   procedure Set_Source_Status (SF : SF_Id; S : SF_Status);
   --  Queries and updates the source status.

   function  Source_Info     (SF : SF_Id) return SF_Info;
   procedure Set_Source_Info (SF : SF_Id; Info : SF_Info);
   --  Queries and updates the source Info value. The use of this value is up
   --  to the client of the source file table. You can store some integer-coded
   --  information or you can use this value as an index value in some other
   --  structure.

   Ignore_Unit : constant SF_Info := 1;
   --  Used to mark units to be ignored in the source table.

   procedure Set_Exemption (Fname : String);
   --  Marks the argument file in the source table as exempted (depending on
   --  the tool, either the file is not processed or no result is generated
   --  for the tool). Generates a warning if Fname does not point to argument
   --  file).

   procedure Process_Exemptions (File_List_Name : String);
   --  Reads the content of the text file that contains a list of the units to
   --  be exempts/ignored and marks the corresponding units in the source
   --  table.

   function Get_Result_Dir (SF : SF_Id) return String;
   procedure Set_Result_Dir
     (SF   : SF_Id;
      Path : String);
   --  Gets/stores a path to the directory where per-file results should be
   --  placed in.

   function Get_Compiler_Out_File_Name (SF : SF_Id) return String;
   --  Gets the name of a temporary file used to redirect the compiler output
   --  into. In case of gnatcheck we have to analyze the messages generated by
   --  each compilation issued for tree creation because the compiler error
   --  messages are included into gnatcheck report. The returned file names are
   --  different for different argument sources,

   -----------------------------
   --  Temporary file storage --
   -----------------------------

   --  We use an ordered set for temporary file storage to ensure as much
   --  determinism in the tool output as possible (in case if a tool prints out
   --  the results and/or diagnoses on per-file basis).

   function File_Name_Is_Less_Than (L, R : String) return Boolean;
   --  Assuming that L and R are file names compares them as follows:
   --
   --  * first, we compare lengths of L and R Base_Names. The reason is to
   --    have in source processing bodies being processed before their subunits
   --    (if any). This is important for gnatcheck, because if we have a
   --    generic instantiation in a separate body, the tree created for this
   --    separate body does not contains the structures for expanded body,
   --    but the tree for enclosing body does. So we have to process a subunit
   --    from the tree created for expanded body
   --
   --  then:
   --
   --  * if L and/or R contains a directory separator, compares
   --    lexicographicaly parts that follow the rightmost directory separator.
   --    If these parts are equal, compares L and R lexicographicaly
   --
   --  * otherwise compares L and R lexicographicaly
   --
   --  Comparisons are case-sensitive.

   package Temporary_File_Storages is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => String,
        "<"          => File_Name_Is_Less_Than);

   ----------------------
   -- Problem counters --
   ----------------------

   Illegal_Sources   : Natural := 0;

   Tool_Failures     : Natural := 0;
   --  Counter for tool failures a tool has recovered from

end Gnatcheck.Source_Table;
