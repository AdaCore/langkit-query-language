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

with Ada.Characters.Conversions;  use Ada.Characters.Conversions;
with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings;                 use Ada.Strings;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Text_IO;                 use Ada.Text_IO;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with GNAT.Table;
with GNAT.Task_Lock;

with GNATCOLL.VFS;                use GNATCOLL.VFS;

with Gnatcheck.Diagnoses;         use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;               use Gnatcheck.Ids;
with Gnatcheck.Options;           use Gnatcheck.Options;
with Gnatcheck.Output;            use Gnatcheck.Output;
with Gnatcheck.Rules;             use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table;  use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.String_Utilities;  use Gnatcheck.String_Utilities;

with Langkit_Support.Images;      use Langkit_Support.Images;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Text;        use Langkit_Support.Text;
with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Helpers;          use Libadalang.Helpers;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Primitives; use LKQL.Primitives;

with LKQL.Errors; use LKQL.Errors;

package body Gnatcheck.Source_Table is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   More_Then_One_Arg_File_Specified : Boolean := False;
   Arg_File_Name                    : String_Access;

   use Temporary_File_Storages;

   Temporary_File_Storage : Temporary_File_Storages.Set;

   -----------------------
   -- Source File table --
   -----------------------

   type String_Loc is access all String;

   type SF_Record is record
      Source_Name  : String_Loc;
      --  Stores the source name with full directory information in absolute
      --  form.

      Short_Source_Name : String_Loc;
      --  The source name without directory information

      Suffixless_Name : String_Loc;
      --  The source name without directory information and suffix (if any)
      --  is used to create the names of the tree file and ALI files

      CU_Name : String_Loc;
      --  The (full expanded) Ada name of a compilation unit contained in the
      --  source, is set to Nil_String_Loc if the unit name is unknown at the
      --  moment or if the source file does not contain a legal unit.

      Status : SF_Status;
      --  Status of the given source. Initially is set to Waiting, then is
      --  changed according to the results of the metrics computation

      Unit_Part : SF_Unit_Parts;
      --  Useful only if argument project is specified!

      Hash_Link : SF_Id;
      --  Link to next entry in files table for same hash code

      Info : SF_Info;
      --  An integer value associated with each source. The usage is up to a
      --  client.

      Switches : String_List_Access;
      --  Used only if a project file is processed as a tool argument. Contains
      --  the list of options to be passed to the compiler to create the tree.

      Result_Dir : String_Access;
      --  Used only if a project file is processed as a tool argument. Contains
      --  the path to the directory the per-source results should be placed in.
   end record;

   package Source_File_Table is new GNAT.Table (
     Table_Component_Type => SF_Record,
     Table_Index_Type     => SF_Id,
     Table_Low_Bound      => First_SF_Id,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Source file table");

   Source_Table : Source_File_Table.Table_Ptr renames Source_File_Table.Table;

   Last_Arg_Source : SF_Id := No_SF_Id;
   --  Used to store the Id of the last argument source

   Short_Source_Name_String : String_Access;
   Full_Source_Name_String  : String_Access;
   --  Two handlers for a file name (with no path information and with full
   --  absolute path) used for the file before we decide that the file should
   --  be stored into a file table. Also used in File_Find for storing the
   --  short file name to be passed into Hash function.

   New_SF_Record : constant SF_Record :=
     (Source_Name       => null,
      Short_Source_Name => null,
      Suffixless_Name   => null,
      CU_Name           => null,
      Status            => Waiting,
      Unit_Part         => Unknown,
      Hash_Link         => No_SF_Id,
      Switches          => null,
      Result_Dir        => null,
      Info              => 0);
   --  Used to set the initial attributes for the new source file

   --  Hash function is the same as in Namet, the only difference is the way
   --  it takes the argument to compute the hash value:

   Hash_Num : constant Integer := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant Integer := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Integer range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of SF_Id := (others => No_SF_Id);
   --  The hash table is used to locate existing entries in the files table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   function Hash (File_Name : String) return Hash_Index_Type;
   --  Compute hash code for the file name. The argument should be a short
   --  file name with no directory information

   function Same_Name_File_Find (Short_SF_Name : String) return SF_Id;
   --  Similar to File_Find, but looks for the file with the same short name.

   function Non_Case_Sensitive_File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False)
      return           SF_Id;
   --  Used as a part of the implementation of File_Find. Tries to locate the
   --  argument in the source table when all the path/file names are converted
   --  to lower case.

   generic
      with procedure Process_File (Fname : String);
   procedure Parse_File_List (File_List_Name : String);
   --  Provided that File_List_Name is a name of some existing file, and
   --  assuming that this file contains a list of source file names, this
   --  procedure parses the argument file and applies Process_File to each file
   --  name.
   --
   --  This procedure assumes that the list of source file names in the
   --  argument file has the following structure:
   --  - file names are separated by white spaces, line breaks, page breaks,
   --    end of file;
   --  - if a file name contains spaces, it should be surrounded by string
   --    quotes.

   procedure Add_Source_To_Process
     (Fname              : String;
      Arg_Project        : Arg_Project_Type'Class;
      Duplication_Report : Boolean   := True;
      Status             : SF_Status := Waiting;
      Subunits           : Boolean   := False);
   --  Fname is treated as the name of the file to process by the tool. We try
   --  to add this file to the table of files to process. The following checks
   --  are performed:
   --
   --  - First, this routine checks if Fname is the name of some existing file,
   --    and if it is not, generates the corresponding diagnosis and does
   --    nothing more. If Arg_Project represents a project file being a tool
   --    parameter, it uses the corresponding project to check the file
   --    existence (no source search path should be used in this case!).
   --    Otherwise this procedure takes into account the search path for the
   --    Ada sources only if ASIS_UL.Compiler_Options.Source_Search_Path is
   --    set, otherwise it just calls GNAT.OS_Lib.Is_Readable_File (Fname)
   --    and checks the result.
   --
   --  - Then, it checks if we already have stored a file with the same name.
   --    If we have the file with the same name, but from a different
   --    directory, a warning is generated, but the file is added to the file
   --    table (the situation when the metric tool is called to process files
   --    with the same name but from different directories looks strange, but
   --    this may be quite legal and reasonable). But if we have already stored
   --    in the list the name of exactly the same file, we generate the error
   --    message and do not change anything in the list of files.
   --
   --  At this stage we do not know if Fname denotes a compilable Ada source
   --  file.
   --
   --  This procedure tries to detect if this source is the source of a
   --  body unit, and if so, stores this in the corresponding record of the
   --  source table. To define this, it checks the suffix of the file name.
   --  It treats suffixes '.adb' and '.2.ada' as suffixes of body files.
   --
   --  It is supposed to be used as a part of the tool parameter processing
   --  in the following way:
   --
   --      loop
   --         declare
   --            Arg : constant String := Get_Argument (...);
   --         begin
   --            exit when Arg = "";
   --            Add_Source_To_Process (Arg);
   --         end;
   --      end loop;
   --
   --  The Subunits parameter has its effect only if Arg_Project is specified.
   --  If it is ON then only sources that contain subunits are added to the
   --  source table. If this parameter is OFF then only the sources that do
   --  not contain subunits are added. This is needed in
   --  Read_Args_From_Temp_Storage procedure to make sure that all the subunits
   --  are stored after the corresponding enclosing bodies.

   function Next_Non_Processed_Source return SF_Id;
   --  Return the next non processed source file id.

   ----------------------------------------------------------------------
   -- Source file access/update routines not used outside this package --
   ----------------------------------------------------------------------

   procedure Set_Source_Name       (SF : SF_Id; N : String);
   procedure Set_Short_Source_Name (SF : SF_Id; N : String);
   procedure Set_Suffixless_Name   (SF : SF_Id; N : String);

   ---------------------------
   -- Add_Source_To_Process --
   ---------------------------

   procedure Add_Source_To_Process
     (Fname              : String;
      Arg_Project        : Arg_Project_Type'Class;
      Duplication_Report : Boolean := True;
      Status             : SF_Status := Waiting;
      Subunits           : Boolean   := False)
   is
      Old_SF : SF_Id;
      New_SF : SF_Id;

      Hash_Index : Hash_Index_Type;

      First_Idx : Natural;
      Last_Idx  : Natural;

      Res      : Virtual_File;
      Res_Info : File_Info;
      U_Part   : SF_Unit_Parts := Unknown;

   begin
      Free (Full_Source_Name_String);
      Free (Short_Source_Name_String);

      if Is_Regular_File (Fname) then
         Short_Source_Name_String := new String'(Fname);
      elsif Is_Specified (Arg_Project) then
         Res := Create (Arg_Project, +Fname);

         if Res = No_File then
            Free (Short_Source_Name_String);
         else
            Short_Source_Name_String := new String'(Res.Display_Full_Name);
         end if;
      end if;

      if Short_Source_Name_String = null then
         Warning (Fname & " not found");
         return;
      else
         Full_Source_Name_String := new String'
           (Normalize_Pathname
              (Short_Source_Name_String.all,
               Resolve_Links  => False,
               Case_Sensitive => True));

         Free (Short_Source_Name_String);
      end if;

      if Is_Specified (Arg_Project) then
         if Res = No_File then
            Res := Create (Arg_Project, +Full_Source_Name_String.all);
            pragma Assert (Res /= No_File);
         end if;

         Res_Info := Info (Arg_Project, Res);

         case Unit_Part (Res_Info) is
            when Unit_Body =>
               U_Part := Unit_Body;
            when Unit_Spec =>
               U_Part := Unit_Spec;
            when Unit_Separate =>
               U_Part := Unit_Separate;
         end case;

         case U_Part is
            when Unit_Separate =>
               if not Subunits then
                  return;
               end if;
            when others =>
               if Subunits then
                  return;
               end if;
         end case;
      end if;

      Short_Source_Name_String := new String'(Base_Name (Fname));
      Hash_Index := Hash (To_Lower (Short_Source_Name_String.all));

      --  Check if we already have a file with the same short name:

      if Present (Hash_Table (Hash_Index)) then
         Old_SF := File_Find (Full_Source_Name_String.all);

         if Present (Old_SF) then
            --  This means that we have already stored exactly the same file.

            if Duplication_Report then
               Error (Short_Source_Name_String.all & " duplicated");
            end if;

            return;

         else
            Old_SF := Same_Name_File_Find (Full_Source_Name_String.all);

            if Present (Old_SF) then
               Error ("more than one version of "
                 & Short_Source_Name_String.all & " processed");
            end if;
         end if;
      end if;

      --  If we are here, we have to store the file in the table

      Source_File_Table.Append (New_SF_Record);
      Last_Arg_Source := Source_File_Table.Last;
      New_SF          := Last_Arg_Source;

      Set_Source_Unit_Part (New_SF, U_Part);

      if Present (Hash_Table (Hash_Index)) then
         Old_SF := Hash_Table (Hash_Index);

         while Present (Source_Table (Old_SF).Hash_Link) loop
            Old_SF := Source_Table (Old_SF).Hash_Link;
         end loop;

         Source_Table (Old_SF).Hash_Link := New_SF;

      else
         Hash_Table (Hash_Index) := New_SF;
      end if;

      Set_Source_Name (New_SF, Full_Source_Name_String.all);
      Set_Short_Source_Name (New_SF, Short_Source_Name_String.all);
      Set_Source_Status     (New_SF, Status);

      First_Idx := Short_Source_Name_String'First;
      Last_Idx  := Short_Source_Name_String'Last;

      for J in reverse  First_Idx + 1 .. Last_Idx loop
         if Short_Source_Name_String (J) = '.' then
            Last_Idx := J - 1;
            exit;
         end if;
      end loop;

      Set_Suffixless_Name
        (New_SF, Short_Source_Name_String (First_Idx .. Last_Idx));

      Free (Short_Source_Name_String);
      Free (Full_Source_Name_String);
   end Add_Source_To_Process;

   ------------------------------
   -- Adjust_From_Source_Table --
   ------------------------------

   procedure Adjust_From_Source_Table (S : in out String) is
      SF : SF_Id;
   begin
      SF := File_Find (SF_Name => S, Use_Short_Name => True);

      if Present (SF) then
         declare
            Result : constant String := Short_Source_Name (SF);
         begin
            if S'Length = Result'Length then
               S := Result;
            end if;
         end;
      end if;
   end Adjust_From_Source_Table;

   --------------------------
   -- Arg_Source_File_Name --
   --------------------------

   function Arg_Source_File_Name return String is
   begin
      if Arg_File_Name = null then
         return "";
      else
         return Arg_File_Name.all;
      end if;
   end Arg_Source_File_Name;

   -------------
   -- CU_Name --
   -------------

   function CU_Name (SF : SF_Id) return String is
   begin
      return Source_Table (SF).CU_Name.all;
   end CU_Name;

   ----------------------
   -- Exempted_Sources --
   ----------------------

   function Exempted_Sources return Natural is
      Result : Natural := 0;
   begin
      for J in First_SF_Id .. Last_Source loop
         if Source_Info (J) = Ignore_Unit then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Exempted_Sources;

   ---------------
   -- File_Find --
   ---------------

   function File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False;
      Case_Sensitive : Boolean := File_Names_Case_Sensitive) return SF_Id
   is
      Result       : SF_Id := No_SF_Id;
      Next_SF      : SF_Id;
      Base_SF_Name : constant String := Base_Name (SF_Name);
   begin
      Next_SF := Hash_Table (Hash (Base_Name (SF_Name)));

      while Present (Next_SF) loop
         if (Use_Short_Name
             and then Base_SF_Name = Short_Source_Name (Next_SF))
           or else SF_Name = Source_Name (Next_SF)
         then
            Result := Next_SF;
            exit;
         end if;

         Next_SF := Source_Table (Next_SF).Hash_Link;
      end loop;

      if not Present (Result) and then not Case_Sensitive then
         Result := Non_Case_Sensitive_File_Find (SF_Name, Use_Short_Name);
      end if;

      return Result;
   end File_Find;

   ----------------------------
   -- File_Name_Is_Less_Than --
   ----------------------------

   function File_Name_Is_Less_Than (L, R : String) return Boolean is
      L_Last : constant Natural := L'Last;
      R_Last : constant Natural := R'Last;

      L_Dir_Separator : Natural :=
        Index (L, (1 => Directory_Separator), Backward);

      R_Dir_Separator : Natural :=
        Index (R, (1 => Directory_Separator), Backward);

      Base_L : constant String := Base_Name (L);
      Base_R : constant String := Base_Name (R);

   begin
      if Base_L'Length /= Base_R'Length then
         return Base_L'Length < Base_R'Length;
      end if;

      if L_Dir_Separator = 0 and then
         R_Dir_Separator = 0
      then
         return L < R;
      end if;

      if L_Dir_Separator = 0 then
         L_Dir_Separator := L'First;
      end if;

      if R_Dir_Separator = 0 then
         R_Dir_Separator := R'First;
      end if;

      if L (L_Dir_Separator .. L_Last) =
         R (R_Dir_Separator .. R_Last)
      then
         return L < R;
      else
         return L (L_Dir_Separator .. L_Last) < R (R_Dir_Separator .. R_Last);
      end if;

   end File_Name_Is_Less_Than;

   ---------------------------
   -- Files_In_Temp_Storage --
   ---------------------------

   function Files_In_Temp_Storage return Natural is
   begin
      return Natural (Length (Temporary_File_Storage));
   end Files_In_Temp_Storage;

   --------------------------------
   -- First_File_In_Temp_Storage --
   --------------------------------

   function First_File_In_Temp_Storage return String is
   begin
      return Ada.Directories.Simple_Name
        (Element (First (Temporary_File_Storage)));
   end First_File_In_Temp_Storage;

   --------------------------------
   -- Get_Compiler_Out_File_Name --
   --------------------------------

   function Get_Compiler_Out_File_Name (SF : SF_Id) return String is
   begin
      return "COMPILER_OUT_" & Image (Integer (SF));
   end Get_Compiler_Out_File_Name;

   --------------------
   -- Get_Result_Dir --
   --------------------

   function Get_Result_Dir (SF : SF_Id) return String is
   begin
      if Source_Table (SF).Result_Dir = null then
         return "";
      else
         return Source_Table (SF).Result_Dir.all & Directory_Separator;
      end if;
   end Get_Result_Dir;

   ----------
   -- Hash --
   ----------

   --  The code is taken from Namet with small modifications

   function Hash (File_Name : String) return Hash_Index_Type is
      subtype Int_0_12 is Integer range 0 .. 12;
      --  Used to avoid when others on case jump below

      Name_Len    : constant Natural                := File_Name'Length;
      Name_Buffer : constant String (1 .. Name_Len) := To_Lower (File_Name);
      --  This allows us to use from Namet without any change at all

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

   begin

      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Name_Len > 12 then
         Even_Name_Len := (Name_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (Name_Buffer (01))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
           Character'Pos (Name_Buffer (03))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
           Character'Pos (Name_Buffer (05))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
           Character'Pos (Name_Buffer (07))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
           Character'Pos (Name_Buffer (09))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
           Character'Pos (Name_Buffer (11))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_0_12 (Name_Len) is

         when 0 =>
            return 0;

         when 1 =>
            return
               Character'Pos (Name_Buffer (1));

         when 2 =>
            return ((
              Character'Pos (Name_Buffer (1))) * 64 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Name_Buffer (1))) * 16 +
              Character'Pos (Name_Buffer (3))) * 16 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Name_Buffer (1))) * 8 +
              Character'Pos (Name_Buffer (2))) * 8 +
              Character'Pos (Name_Buffer (3))) * 8 +
              Character'Pos (Name_Buffer (4))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Name_Buffer (4))) * 8 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (5))) * 8 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Name_Buffer (5))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (6))) * 4 +
              Character'Pos (Name_Buffer (3))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (2))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (9))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (10))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (11))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (11))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (12))) mod Hash_Num;

      end case;
   end Hash;

   ------------------------
   -- Is_Argument_Source --
   ------------------------

   function Is_Argument_Source (SF : SF_Id) return Boolean is
   begin
      return SF in First_SF_Id .. Last_Argument_Source;
   end Is_Argument_Source;

   ----------------------
   -- Is_Needed_Source --
   ----------------------

   function Is_Needed_Source (SF : SF_Id) return Boolean is
   begin
      return SF in Last_Argument_Source + 1 .. Source_File_Table.Last;
   end Is_Needed_Source;

   -----------------
   -- Last_Source --
   -----------------

   function Last_Source return SF_Id is
   begin
      return Source_File_Table.Last;
   end Last_Source;

   --------------------------
   -- Last_Argument_Source --
   --------------------------

   function Last_Argument_Source return SF_Id is
   begin
      return Last_Arg_Source;
   end Last_Argument_Source;

   -------------------------------
   -- Next_Non_Processed_Source --
   -------------------------------

   Next_Source : SF_Id := First_SF_Id;
   --  Used in source file iterator

   function Next_Non_Processed_Source return SF_Id is
      Move_Next_Source : Boolean := True;
   begin
      GNAT.Task_Lock.Lock;

      for J in Next_Source .. Last_Argument_Source loop
         if Source_Status (J) = Waiting
           and then Source_Info (J) /= Ignore_Unit
         then
            for K in Next_Source + 1 .. J - 1 loop
               if Source_Status (K) in Waiting then
                  Move_Next_Source := False;
                  exit;
               end if;
            end loop;

            if Move_Next_Source then
               Next_Source := J;
            end if;

            Source_Table (J).Status := Processed;
            GNAT.Task_Lock.Unlock;
            return J;
         end if;
      end loop;

      GNAT.Task_Lock.Unlock;
      return No_SF_Id;
   end Next_Non_Processed_Source;

   ----------------------------------
   -- Non_Case_Sensitive_File_Find --
   ----------------------------------

   function Non_Case_Sensitive_File_Find
     (SF_Name        : String;
      Use_Short_Name : Boolean := False)
      return           SF_Id
   is
      Result       : SF_Id := No_SF_Id;
      Next_SF      : SF_Id;
      Base_SF_Name : constant String := To_Lower (Base_Name (SF_Name));
      Arg_Name     : constant String := To_Lower (SF_Name);

   begin
      Next_SF := Hash_Table (Hash (Base_Name (SF_Name)));

      while Present (Next_SF) loop

         if (Use_Short_Name
             and then Base_SF_Name = To_Lower (Short_Source_Name (Next_SF)))
           or else Arg_Name = To_Lower (Source_Name (Next_SF))
         then
            Result := Next_SF;
            exit;
         end if;

         Next_SF := Source_Table (Next_SF).Hash_Link;
      end loop;

      return Result;
   end Non_Case_Sensitive_File_Find;

   -------------------
   -- Output_Source --
   -------------------

   procedure Output_Source (SF : SF_Id) is
      N : constant String := Natural'Image (Sources_Left);
   begin
      if Progress_Indicator_Mode then
         declare
            Current : constant Integer := Total_Sources - Sources_Left + 1;
            Percent : String :=
              Integer'Image ((Current * 100) / Total_Sources);
         begin
            Percent (1) := '(';
            Info ("completed" & Integer'Image (Current) & " out of"
                  & Integer'Image (Total_Sources) & " "
                  & Percent & "%)...");
         end;
      end if;

      if Verbose_Mode then
         Info_No_EOL ("[" & N (2 .. N'Last) & "] ");
         Info (Short_Source_Name (SF));

      elsif not (Quiet_Mode or Progress_Indicator_Mode) then
         Info_No_EOL ("Units remaining:");
         Info_No_EOL (N);
         Info_No_EOL ("     " & ASCII.CR);
      end if;

      Sources_Left := Sources_Left - 1;
   end Output_Source;

   -------------
   -- Present --
   -------------

   function Present (SF : SF_Id) return Boolean is
   begin
      return SF in First_SF_Id .. Source_File_Table.Last;
   end Present;

   -------------------------
   -- Read_Args_From_File --
   -------------------------

   procedure Read_Args_From_File (Par_File_Name : String) is
      Arg_File         : File_Type;
      File_Name_Buffer : String (1 .. 16 * 1024);
      File_Name_Len    : Natural := 0;
      Next_Ch          : Character;
      End_Of_Line      : Boolean;

      function Get_File_Name return String;
      --  Reads from Par_File_Name the name of the next file (the file to read
      --  from should exist and be opened). Returns an empty string if there
      --  are no more file names in Par_File_Name.

      function Get_File_Name return String is
      begin
         File_Name_Len := 0;

         while Ada.Text_IO.End_Of_Line (Arg_File)
            and then
               not End_Of_File (Arg_File)
         loop
            Skip_Line (Arg_File);
         end loop;

         if not End_Of_File (Arg_File) then
            Get (Arg_File, Next_Ch);

            while Next_Ch in ASCII.LF | ASCII.CR loop
               exit when End_Of_File (Arg_File);
               Get (Arg_File, Next_Ch);
            end loop;

            --  If we are here. Next_Ch is neither a white space nor
            --  end-of-line character. Two cases are possible, they require
            --  different processing:
            --
            --  1. Next_Ch = '"', this means that the file name is surrounded
            --     by quotation marks and it can contain spaces inside.
            --
            --  2. Next_Ch /= '"', this means that the file name is bounded by
            --     a white space or end-of-line character

            if Next_Ch = '"' then

               --  We do not generate any warning for badly formatted content
               --  of the file such as
               --
               --    file_name_1
               --    "file name 2
               --    file_name_3
               --
               --  (We do not check that quotation marks correctly go by pairs)

               --  Skip leading '"'
               Get (Arg_File, Next_Ch);

               while not (Next_Ch = '"'
                  or else
                     Next_Ch = ASCII.LF
                  or else
                     Next_Ch = ASCII.CR)
               loop
                  File_Name_Len := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;

               if Next_Ch = '"'
                 and then
                  not Ada.Text_IO.End_Of_Line (Arg_File)
               then
                  --  skip trailing '"'
                  Get (Arg_File, Next_Ch);
               end if;
            else
               while Next_Ch not in ASCII.LF | ASCII.CR loop
                  File_Name_Len := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;
            end if;

         end if;

         return File_Name_Buffer (1 .. File_Name_Len);
      end Get_File_Name;

   --  Start of processing for Read_Args_From_File

   begin
      Gnatcheck.Options.No_Argument_File_Specified := False;

      if not Is_Regular_File (Par_File_Name) then
         Error (Par_File_Name & " does not exist");
         return;
      end if;

      Open (Arg_File, In_File, Par_File_Name);

      loop
         declare
            Tmp_Str : constant String := Get_File_Name;
         begin
            exit when Tmp_Str = "";

            Store_Sources_To_Process (Tmp_Str);
         end;
      end loop;

      if not More_Then_One_Arg_File_Specified then
         if Arg_File_Name /= null then
            --  We have already encountered one non-empty argument file
            Free (Arg_File_Name);
            More_Then_One_Arg_File_Specified := True;
         else
            Arg_File_Name := new String'(Par_File_Name);
         end if;
      end if;

      Close (Arg_File);

   exception
      when others =>
         Error ("cannot read arguments from " & Par_File_Name);
         --  Exception info will be generated in main driver
         raise;
   end Read_Args_From_File;

   --------------------------
   -- Temp_Storage_Iterate --
   --------------------------

   procedure Temp_Storage_Iterate
     (Action : not null access procedure (File_Name : String)) is
      C : Temporary_File_Storages.Cursor := First (Temporary_File_Storage);
   begin
      while C /= No_Element loop
         Action (Element (C));
         C := Next (C);
      end loop;
   end Temp_Storage_Iterate;

   ---------------------------------
   -- Read_Args_From_Temp_Storage --
   ---------------------------------

   procedure Read_Args_From_Temp_Storage
     (Duplication_Report : Boolean;
      Arg_Project        : Arg_Project_Type'Class;
      Status             : SF_Status := Waiting)
   is
      Subunits : Boolean := False;

      procedure Action (File_Name : String);
      procedure Action (File_Name : String) is
      begin
         Add_Source_To_Process
           (Fname              => File_Name,
            Arg_Project        => Arg_Project,
            Duplication_Report => Duplication_Report,
            Status             => Status,
            Subunits           => Subunits);
      end Action;

   begin
      Temp_Storage_Iterate (Action'Access);

      if Is_Specified (Arg_Project) then
         Subunits := True;
         Temp_Storage_Iterate (Action'Access);
      end if;

      Clear (Temporary_File_Storage);
   end Read_Args_From_Temp_Storage;

   -------------------------
   -- Same_Name_File_Find --
   -------------------------

   function Same_Name_File_Find (Short_SF_Name : String) return SF_Id is
      Result     : SF_Id := No_SF_Id;
      Next_SF    : SF_Id;
   begin
      Next_SF := Hash_Table (Hash (Short_SF_Name));

      while Present (Next_SF) loop

         if Short_SF_Name = Short_Source_Name (Next_SF) then
            Result := Next_SF;
            exit;
         end if;

         Next_SF := Source_Table (Next_SF).Hash_Link;
      end loop;

      return Result;
   end Same_Name_File_Find;

   -----------------
   -- Set_CU_Name --
   -----------------

   procedure Set_CU_Name (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).CU_Name := new String'(N);
   end Set_CU_Name;

   --------------------
   -- Set_Result_Dir --
   --------------------

   procedure Set_Result_Dir (SF : SF_Id; Path : String) is
   begin
      Source_Table (SF).Result_Dir := new String'(Path);
   end Set_Result_Dir;

   ---------------------
   -- Set_Source_Info --
   ---------------------

   procedure Set_Source_Info (SF : SF_Id; Info : SF_Info) is
   begin
      Source_Table (SF).Info := Info;
   end Set_Source_Info;

   ---------------------------
   -- Set_Short_Source_Name --
   ---------------------------

   procedure Set_Short_Source_Name (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).Short_Source_Name := new String'(N);
   end Set_Short_Source_Name;

   ---------------------
   -- Set_Source_Name --
   ---------------------

   procedure Set_Source_Name (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).Source_Name := new String'(N);
   end Set_Source_Name;

   -----------------
   -- Source_Info --
   -----------------

   function Source_Info (SF : SF_Id) return SF_Info is
   begin
      return Source_Table (SF).Info;
   end Source_Info;

   -------------------
   -- Set_Exemption --
   -------------------

   procedure Set_Exemption (Fname : String) is
      SF : constant SF_Id := File_Find (Fname, Use_Short_Name => True);
   begin
      if Present (SF) then
         Set_Source_Info (SF, Ignore_Unit);
      else
         Gnatcheck.Output.Warning
           ("exemption: source " & Fname & " not found");
      end if;
   end Set_Exemption;

   -----------------------
   -- Set_Source_Status --
   -----------------------

   procedure Set_Source_Status (SF : SF_Id; S : SF_Status) is
   begin
      GNAT.Task_Lock.Lock;
      Source_Table (SF).Status := S;

      case S is
         when Not_A_Legal_Source =>
            Illegal_Sources := Illegal_Sources + 1;
         when Error_Detected =>
            Tool_Failures := Tool_Failures + 1;
         when others =>
            null;
      end case;

      GNAT.Task_Lock.Unlock;
   end Set_Source_Status;

   --------------------------
   -- Set_Source_Unit_Part --
   --------------------------

   procedure Set_Source_Unit_Part (SF : SF_Id; Unit_Part : SF_Unit_Parts) is
   begin
      Source_Table (SF).Unit_Part := Unit_Part;
   end Set_Source_Unit_Part;

   -------------------------
   -- Set_Suffixless_Name --
   -------------------------

   procedure Set_Suffixless_Name   (SF : SF_Id; N : String) is
   begin
      Source_Table (SF).Suffixless_Name := new String'(N);
   end Set_Suffixless_Name;

   -----------------------
   -- Short_Source_Name --
   -----------------------

   function Short_Source_Name (SF : SF_Id) return String is
   begin
      return Source_Table (SF).Short_Source_Name.all;
   end Short_Source_Name;

   ---------------------
   -- Source_Clean_Up --
   ---------------------

   procedure Source_Clean_Up
     (SF             : SF_Id;
      Keep_ALI_Files : Boolean := False)
   is
      Success : Boolean;
   begin
      if not Keep_ALI_Files then
         Delete_File (Suffixless_Name (SF) & ".ali", Success);
      end if;
   end Source_Clean_Up;

   -----------------
   -- Source_Name --
   -----------------

   function Source_Name (SF : SF_Id) return String is
   begin
      return Source_Table (SF).Source_Name.all;
   end Source_Name;

   -------------------
   -- Source_Status --
   -------------------

   function Source_Status (SF : SF_Id) return SF_Status is
   begin
      return Source_Table (SF).Status;
   end Source_Status;

   ----------------------
   -- Source_Unit_Part --
   ----------------------

   function Source_Unit_Part (SF : SF_Id) return SF_Unit_Parts is
   begin
      return Source_Table (SF).Unit_Part;
   end Source_Unit_Part;

   ------------------------------
   -- Store_Sources_To_Process --
   ------------------------------

   procedure Store_Sources_To_Process
     (Fname : String;
      Store : Boolean := True) is
   begin
      Gnatcheck.Options.No_Argument_File_Specified := False;

      if Store then
         Include (Temporary_File_Storage, Fname);
      end if;
   end Store_Sources_To_Process;

   ---------------------
   -- Suffixless_Name --
   ---------------------

   function Suffixless_Name (SF : SF_Id) return String is
   begin
      return Source_Table (SF).Suffixless_Name.all;
   end Suffixless_Name;

   ------------------------------
   -- Total_Sources_To_Process --
   ------------------------------

   function Total_Sources_To_Process return Natural is
      Result : Natural := 0;
   begin
      for J in First_SF_Id .. Last_Source loop
         if Source_Info (J) /= Ignore_Unit then
            Result := Result + 1;
         end if;
      end loop;

      return Result;
   end Total_Sources_To_Process;

   ---------------------
   -- Parse_File_List --
   ---------------------

   procedure Parse_File_List (File_List_Name : String) is
      Arg_File         : File_Type;
      File_Name_Buffer : String (1 .. 16 * 1024);
      File_Name_Len    : Natural := 0;
      Next_Ch          : Character;
      End_Of_Line      : Boolean;
      Tmp_Str          : String_Access;

      function Get_File_Name return String;
      --  Reads from Par_File_Name the name of the next file (the file to read
      --  from should exist and be opened). Returns an empty string if there is
      --  no file names in Par_File_Name any more

      function Get_File_Name return String is
      begin
         File_Name_Len := 0;

         while Ada.Text_IO.End_Of_Line (Arg_File)
            and then
               not End_Of_File (Arg_File)
         loop
            Skip_Line (Arg_File);
         end loop;

         if not End_Of_File (Arg_File) then
            Get (Arg_File, Next_Ch);

            while Is_White_Space (Next_Ch)
               or else
                  Next_Ch = ASCII.LF
               or else
                  Next_Ch = ASCII.CR
            loop
               exit when End_Of_File (Arg_File);
               Get (Arg_File, Next_Ch);
            end loop;

            --  If we are here. Next_Ch is neither a white space nor
            --  end-of-line character. Two cases are possible, they require
            --  different processing:
            --
            --  1. Next_Ch = '"', this means that the file name is surrounded
            --     by quotation marks and it can contain spaces inside.
            --
            --  2. Next_Ch /= '"', this means that the file name is bounded by
            --     a white space or end-of-line character

            if Next_Ch = '"' then

               --  We do not generate any warning for badly formatted content
               --  of the file such as
               --
               --    file_name_1
               --    "file name 2
               --    file_name_3
               --
               --  (We do not check that quotation marks correctly go by pairs)

               --  Skip leading '"'
               Get (Arg_File, Next_Ch);

               while not (Next_Ch in '"' | ASCII.LF | ASCII.CR) loop
                  File_Name_Len := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;

               if Next_Ch = '"'
                 and then
                  not Ada.Text_IO.End_Of_Line (Arg_File)
               then
                  --  skip trailing '"'
                  Get (Arg_File, Next_Ch);
               end if;
            else
               while Next_Ch not in ASCII.LF | ASCII.CR | ASCII.HT | ' ' loop
                  File_Name_Len := File_Name_Len + 1;
                  File_Name_Buffer (File_Name_Len) := Next_Ch;

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;
            end if;
         end if;

         return File_Name_Buffer (1 .. File_Name_Len);
      end Get_File_Name;

   begin
      Open (Arg_File, In_File, File_List_Name);
      Tmp_Str := new String'(Get_File_Name);

      while Tmp_Str.all /= "" loop
         Process_File (Tmp_Str.all);
         Free (Tmp_Str);
         Tmp_Str := new String'(Get_File_Name);
      end loop;

      Free (Tmp_Str);
   end Parse_File_List;

   ------------------------
   -- Process_Exemptions --
   ------------------------

   procedure Process_Exemptions (File_List_Name : String) is
      procedure Local_Process_Exemptions is new
        Parse_File_List (Set_Exemption);
   begin
      Local_Process_Exemptions (File_List_Name);
   end Process_Exemptions;

   ---------------------
   -- Process_Sources --
   ---------------------

   procedure Process_Sources (Ctx : LKQL_Context) is
      Next_SF : SF_Id;
      Cached_Rule_Id : Rule_Id;
      Cached_Rule    : Unbounded_Text_Type;

      procedure Store_Message
        (Message    : Unbounded_Text_Type;
         Unit       : Analysis_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range);
      --  Callback to store messages

      -------------------
      -- Store_Message --
      -------------------

      procedure Store_Message
        (Message    : Unbounded_Text_Type;
         Unit       : Analysis_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range)
      is
         Id : Rule_Id;

         use Ada.Directories;

         function Column_Image (Column : Natural) return String;
         --  Return an image of Column with no leading space and a leading '0'
         --  if column is less than 10.

         ------------------
         -- Column_Image --
         ------------------

         function Column_Image (Column : Natural) return String is
            Image : constant String := Column'Image;
         begin
            if Column < 10 then
               return "0" & Image (2 .. Image'Last);
            else
               return Image (2 .. Image'Last);
            end if;
         end Column_Image;

      begin
         GNAT.Task_Lock.Lock;

         if Rule = Cached_Rule then
            Id := Cached_Rule_Id;
         else
            Id             := Get_Rule (To_String (To_Text (Rule)));
            Cached_Rule_Id := Id;
            Cached_Rule    := Rule;
         end if;

         Store_Diagnosis
           (Text           =>
              (if Full_Source_Locations
               then Unit.Get_Filename
               else Simple_Name (Unit.Get_Filename)) & ":" &
              Stripped_Image (Integer (Sloc_Range.Start_Line)) & ":" &
              Column_Image (Natural (Sloc_Range.Start_Column)) & ": " &
              To_String (To_Text (Message)) &
              Annotate_Rule (All_Rules.Table (Id).all),
            Diagnosis_Kind => (case Kind is
                               when Rule_Violation => Rule_Violation,
                               when Internal_Error => Compiler_Error),
            SF             => Next_SF,
            Rule           => Id);
         GNAT.Task_Lock.Unlock;
      end Store_Message;

   begin
      loop
         Next_SF := Next_Non_Processed_Source;

         exit when not Present (Next_SF);

         --  ### Revisit handling of postponed exemptions
         --  Gnatcheck.Diagnoses.Init_Postponed_Check_Exemptions;

         Output_Source (Next_SF);
         Process_Unit
           (Ctx,
            Ctx.Analysis_Ctx.Get_From_File (Source_Name (Next_SF)),
            Store_Message'Access);
      end loop;
   end Process_Sources;

   --------------------
   -- Create_Context --
   --------------------

   Partition : Provider_And_Projects_Array_Access;

   function Create_Context return LKQL_Context is
      Ctx       : LKQL_Context;
      Dummy     : Primitive;
      Units     : Unit_Vectors.Vector;

   begin
      if Partition = null then
         Partition := Create_Project_Unit_Providers (Gnatcheck_Prj'Access);
      end if;

      --  Reject partitions with multiple parts: we cannot analyze it with
      --  only one provider.

      if Partition'Length /= 1 then
         Free (Partition);
         Error ("This aggregate project contains conflicting sources");
         raise Fatal_Error;
      end if;

      Ctx.Analysis_Ctx := Create_Context
        (Charset       => "iso-8859-1",   --  ### or UTF-8
         Unit_Provider => Partition (Partition'First).Provider);

      GNAT.Task_Lock.Lock;
      for J in First_SF_Id .. Last_Argument_Source loop
         if Source_Info (J) /= Ignore_Unit then
            Units.Append (Ctx.Analysis_Ctx.Get_From_File (Source_Name (J)));
         end if;
      end loop;
      GNAT.Task_Lock.Unlock;

      Ctx.Eval_Ctx := Make_Eval_Context (Units);
      LKQL.Errors.Property_Error_Recovery := LKQL.Errors.Continue_And_Warn;

      return Ctx;
   end Create_Context;

end Gnatcheck.Source_Table;
