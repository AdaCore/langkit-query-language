--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Expect;                use GNAT.Expect;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.String_Split;          use GNAT.String_Split;
with GNAT.Table;
with GNAT.Task_Lock;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.Diagnoses;        use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GPR2.Build.Source;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Generic_API.Introspection;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Auto_Provider;    use Libadalang.Auto_Provider;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;
with Libadalang.Iterators;
with Libadalang.Generic_API;      use Libadalang.Generic_API;
with Libadalang.Common;
with Libadalang.Config_Pragmas;

with Liblkqllang.Analysis;

package body Gnatcheck.Source_Table is

   package LKI renames Langkit_Support.Generic_API.Introspection;

   subtype String_Access is GNAT.OS_Lib.String_Access;

   Arg_File_Name : String_Access;

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

      Hash_Link : SF_Id;
      --  Link to next entry in files table for same hash code

      Info : SF_Info;
      --  An integer value associated with each source. The usage is up to a
      --  client.

      Switches : String_List_Access;
      --  Used only if a project file is processed as a tool argument. Contains
      --  the list of options to be passed to the compiler to create the tree.
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
      Hash_Link         => No_SF_Id,
      Switches          => null,
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

   Hash_Table : array (Hash_Index_Type) of SF_Id := [others => No_SF_Id];
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
      Use_Short_Name : Boolean := False) return SF_Id;
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
   --  - lines starting with -- are ignored (comments)

   procedure Add_Source_To_Process
     (Fname              : String;
      Arg_Project        : Arg_Project_Type'Class;
      Duplication_Report : Boolean   := True;
      Status             : SF_Status := Waiting);
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

   function Next_Non_Processed_Source return SF_Id;
   --  Return the next non processed source file id.

   type EHI is new Event_Handler_Interface with null record;

   procedure Release (Self : in out EHI) is null;
   --  No resources associated to Self to release, so just a stub

   EHI_Obj : EHI;

   EHR_Object : constant Event_Handler_Reference :=
     Create_Event_Handler_Reference (EHI_Obj);

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
      Status             : SF_Status := Waiting)
   is
      use GPR2;
      use GPR2.Project.Tree;

      Old_SF : SF_Id;
      New_SF : SF_Id;

      Hash_Index : Hash_Index_Type;

      First_Idx : Natural;
      Last_Idx  : Natural;

      Root : constant GPR2.Project.View.Object :=
        Arg_Project.Tree.Namespace_Root_Projects.First_Element;
      Res : constant GPR2.Build.Source.Object :=
        Root.View_Db.Visible_Source
          (GPR2.Path_Name.Simple_Name (Filename_Type (Fname)));
   begin
      Free (Full_Source_Name_String);
      Free (Short_Source_Name_String);

      if not Res.Is_Defined then
         if Is_Regular_File (Fname) then
            Warning (Fname & " is not in the analysed project closure (" &
                     String (Arg_Project.Tree.Root_Project.Name) & ")");
         else
            Warning (Fname & " not found");
         end if;
         Missing_File_Detected := True;
         return;
      else
         Short_Source_Name_String :=
           new String'(Res.Path_Name.String_Value);
      end if;

      Full_Source_Name_String := new String'
        (Normalize_Pathname
           (Short_Source_Name_String.all,
            Resolve_Links  => False,
            Case_Sensitive => True));
      Free (Short_Source_Name_String);

      Short_Source_Name_String := new String'(Base_Name (Fname));
      Hash_Index := Hash (To_Lower (Short_Source_Name_String.all));

      --  Check if we already have a file with the same short name:
      if Present (Hash_Table (Hash_Index)) then
         Old_SF := File_Find (Full_Source_Name_String.all);

         --  Check if we already stored exactly the same file.
         if Present (Old_SF) then
            if Duplication_Report then
               Error (Short_Source_Name_String.all & " duplicated");
            end if;
            return;

         --  Else, look for files with the same name but with a difference path
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
        Index (L, [Directory_Separator], Backward);

      R_Dir_Separator : Natural :=
        Index (R, [Directory_Separator], Backward);

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
      if Arg.Progress_Indicator_Mode.Get then
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

      elsif not (Arg.Quiet_Mode or Arg.Progress_Indicator_Mode.Get) then
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
      if Argument_File_Specified then
         Error ("cannot specify more than one -file");
         return;
      end if;

      Argument_File_Specified := True;
      Arg_File_Name := new String'(Par_File_Name);

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
      procedure Action (File_Name : String);
      procedure Action (File_Name : String) is
      begin
         Add_Source_To_Process
           (Fname              => File_Name,
            Arg_Project        => Arg_Project,
            Duplication_Report => Duplication_Report,
            Status             => Status);
      end Action;

   begin
      Temp_Storage_Iterate (Action'Access);
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

      --  Only warn if no sources are specified explicitly

      elsif not (File_List_Specified
                 or else (Argument_File_Specified
                          and then not Arg.Transitive_Closure.Get))
      then
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

   ------------------------------
   -- Store_Sources_To_Process --
   ------------------------------

   procedure Store_Sources_To_Process
     (Fname : String;
      Store : Boolean := True) is
   begin
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
      subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

      Arg_File         : File_Type;
      Next_Ch          : Character;
      End_Of_Line      : Boolean;
      Tmp_Str          : Unbounded_String;

      function Get_File_Name return Unbounded_String;
      --  Reads from Par_File_Name the name of the next file (the file to read
      --  from should exist and be opened). Returns Null_Unbounded_String if
      --  there is no file in Par_File_Name any more.

      function Get_File_Name return Unbounded_String is
         Result : Unbounded_String;

         procedure Compute_File_Name;
         --  Compute file name and store it in File_Name_Buffer

         procedure Compute_File_Name is
         begin
            while Next_Ch not in ASCII.LF | ASCII.CR | ASCII.HT | ' ' loop
               Append (Result, Next_Ch);
               Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

               exit when End_Of_Line or else End_Of_File (Arg_File);

               Get (Arg_File, Next_Ch);
            end loop;
         end Compute_File_Name;

      begin
         while Ada.Text_IO.End_Of_Line (Arg_File)
           and then not End_Of_File (Arg_File)
         loop
            Skip_Line (Arg_File);
         end loop;

         while not End_Of_File (Arg_File) loop
            Get (Arg_File, Next_Ch);

            while Next_Ch in ASCII.LF | ASCII.CR | ASCII.HT | ' ' loop
               exit when End_Of_File (Arg_File);
               Get (Arg_File, Next_Ch);
            end loop;

            --  If we are here. Next_Ch is neither a white space nor
            --  end-of-line character. Three cases are possible:
            --
            --  1. Next_Ch = '"', this means that the file name is surrounded
            --     by quotation marks and it can contain spaces inside.
            --
            --  2. Next_Ch = '-', followed by another '-', this is a comment,
            --     and all characters until the end of line will be ignored.
            --
            --  3. Otherwise we have a file name ending with a white space or
            --     end-of-line character.

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

               while Next_Ch not in '"' | ASCII.LF | ASCII.CR loop
                  Append (Result, Next_Ch);

                  Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

                  exit when End_Of_Line or else End_Of_File (Arg_File);

                  Get (Arg_File, Next_Ch);
               end loop;

               if Next_Ch = '"'
                 and then not Ada.Text_IO.End_Of_Line (Arg_File)
               then
                  --  skip trailing '"'
                  Get (Arg_File, Next_Ch);
               end if;

               exit;

            elsif Next_Ch = '-' then
               Look_Ahead (Arg_File, Next_Ch, End_Of_Line);

               if Next_Ch = '-' then
                  Skip_Line (Arg_File);
               else
                  Compute_File_Name;
                  exit;
               end if;
            else
               Compute_File_Name;
               exit;
            end if;
         end loop;

         return Result;
      end Get_File_Name;

   begin
      Open (Arg_File, In_File, File_List_Name);
      Tmp_Str := Get_File_Name;

      while Tmp_Str /= Null_Unbounded_String loop
         Process_File (To_String (Tmp_Str));
         Tmp_Str := Get_File_Name;
      end loop;
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

   procedure Process_Sources
     (Ctx : Checker_App.Lkql_Context)
   is
      Next_SF : SF_Id;

      use Libadalang.Iterators;

      function Strip_LF (S : String) return String is
      (if S (S'Last) = ASCII.LF then S (S'First .. S'Last - 1) else S);
      --  Remove trailing LF if any

   begin
      loop
         Next_SF := Next_Non_Processed_Source;

         exit when not Present (Next_SF);

         declare
            Unit : constant Analysis_Unit :=
              Ctx.Analysis_Ctx.Get_From_File (Source_Name (Next_SF));
         begin
            --  Process exemption pragmas for Unit

            declare
               It      : Traverse_Iterator'Class := Traverse (Unit.Root);
               Current : Ada_Node;
               Dummy   : constant Boolean := It.Next (Current);

               use Libadalang.Common;
            begin
               while It.Next (Current) loop
                  if Current.Kind = Ada_Pragma_Node
                    and then Is_Exemption_Pragma (Current.As_Pragma_Node)
                  then
                     Process_Exemption_Pragma (Current.As_Pragma_Node);
                  end if;
               end loop;
            end;

            --  Process exemption comments for Unit

            declare
               use Libadalang.Common;
               TR : Token_Reference := Unit.First_Token;
            begin
               while TR /= No_Token loop
                  if Kind (Data (TR)) = Ada_Comment then
                     Process_Exemption_Comment (TR, Unit);
                  end if;
                  TR := Next (TR);
               end loop;
            end;

            Check_Unclosed_Rule_Exemptions (Next_SF, Unit);
         exception
            when E : others =>
               if Arg.Debug_Mode.Get then
                  declare
                     Msg : constant String :=
                       File_Name (Next_SF) & ":1:01: internal error: " &
                       Strip_LF (Exception_Information (E));
                  begin
                     Store_Diagnosis
                       (Text           => Msg,
                        Diagnosis_Kind => Internal_Error,
                        SF             => Next_SF,
                        Rule           => No_Rule_Id);
                  end;
               end if;
         end;
      end loop;
   end Process_Sources;

   --------------------
   -- Create_Context --
   --------------------

   Partition : GPR2_Provider_And_Projects_Array_Access;

   function Create_Context return Checker_App.Lkql_Context is
      Ctx   : Checker_App.Lkql_Context;
      Files : File_Array_Access;
      Last  : Natural := 0;

      procedure Add_Runtime_Files;
      --  Add to Files all the GNAT native runtime files, if found

      -----------------------
      -- Add_Runtime_Files --
      -----------------------

      procedure Add_Runtime_Files is
         Gnatls  : String_Access := Locate_Exec_On_Path (Gnatls_Exec);
         Verbose : aliased String := "-v";
         Status  : aliased Integer;

      begin
         if Gnatls = null then
            return;
         end if;

         --  Spawn gnatls -v

         declare
            use Ada.Directories;

            Output : constant String :=
              Get_Command_Output (Gnatls.all,
                                  [Verbose'Unchecked_Access],
                                  "", Status'Unchecked_Access, True);
            Lines : String_List_Access;
            Ada_Include_Path : String_Access;
            Found : Boolean := False;

            procedure Add_File (Dir : Directory_Entry_Type);
            --  Add the given directory entry Dir to Files

            --------------
            -- Add_File --
            --------------

            procedure Add_File (Dir : Directory_Entry_Type) is
            begin
               Last := @ + 1;
               Files (Last) := Create (+Full_Name (Dir));
            end Add_File;

         begin
            if Status /= 0 then
               Free (Gnatls);
               return;
            end if;

            --  and look for the line containing "adainclude"

            for Line of Create (Output, [ASCII.LF, ASCII.CR], Multiple)
            loop
               Found := Has_Suffix (Line, "adainclude");

               if Found then
                  Ada_Include_Path :=
                    new String'(Remove_Spaces (Line));
                  exit;
               end if;
            end loop;

            Free (Lines);

            if not Found then
               Free (Gnatls);
               return;
            end if;

            --  We then list all the *.ads files.
            --  We only need to process spec files, runtime body files are not
            --  needed to analyze user code and will slow down the startup
            --  phase.

            Search (Ada_Include_Path.all, "*.ads",
                    Process => Add_File'Access);
            Free (Ada_Include_Path);
            Free (Gnatls);
         end;
      end Add_Runtime_Files;

      Charset : constant String := To_String (Arg.Charset.Get);

   begin
      --  If no project specified, create an auto provider with all the source
      --  files listed in the command line, stored in Temporary_File_Storage,
      --  as well as all runtime files, these are needed for proper name
      --  resolution.

      if not Gnatcheck_Prj.Is_Specified then
         declare
            procedure Add_File (File_Name : String);
            --  Add File_Name to Files

            --------------
            -- Add_File --
            --------------

            procedure Add_File (File_Name : String) is
            begin
               Last := @ + 1;
               Files (Last) := Create (+File_Name);
            end Add_File;

         begin
            Files := new File_Array
                      (1 .. Natural (Length (Temporary_File_Storage)) + 4096);
            --  Enough to hold all files on the command line and all runtime
            --  files.

            Temp_Storage_Iterate (Add_File'Access);
            Add_Runtime_Files;
            Ctx.Analysis_Ctx := Create_Context
              (Charset       => Charset,
               Unit_Provider => Create_Auto_Provider_Reference
                                  (Files (1 .. Last), Charset),
               Event_Handler => EHR_Object);
            Unchecked_Free (Files);
         end;

      --  Otherwise use a project unit provider

      elsif  Gnatcheck_Prj.Tree.Is_Defined and then not In_Aggregate_Project
      then
         if Partition = null then
            Partition :=
              Create_Project_Unit_Providers (Gnatcheck_Prj.Tree);
         end if;

         --  We can ignore multiple partitions: this will only occur with
         --  aggregate projects, which are handled specially in lalcheck.adb

         Ctx.Analysis_Ctx := Create_Context
           (Charset       => Charset,
            Unit_Provider => Partition (Partition'First).Provider,
            Event_Handler => EHR_Object);

         --  Setup the configuration pragma mapping by reading the
         --  configuration file given by the project.
         Libadalang.Config_Pragmas.Import_From_Project
           (Ctx.Analysis_Ctx, Gnatcheck_Prj.Tree);
      end if;

      --  Initialize the cached rules array, with an array that goes from
      --  the index of the first root node type, to the index of the last
      --  derived type. This array will have too many slots since is has
      --  slots for abstract types, but we don't really care.
      declare
         use Checker_App;

         Root_Node_Type : LKI.Type_Ref
           renames LKI.Root_Node_Type (Ada_Lang_Id);
         subtype Rules_By_Kind_Array_Subt is
           Rules_By_Kind_Array
             (LKI.To_Index (Root_Node_Type)
              .. LKI.Last_Derived_Type (Root_Node_Type));

      begin
         Ctx.Cached_Rules := new Rules_By_Kind_Array_Subt;
      end;

      Ctx.LKQL_Analysis_Context := Liblkqllang.Analysis.Create_Context
        (Charset => "utf-8");

      return Ctx;

   end Create_Context;

end Gnatcheck.Source_Table;
