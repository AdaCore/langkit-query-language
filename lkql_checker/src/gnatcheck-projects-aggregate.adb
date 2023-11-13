------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

package body Gnatcheck.Projects.Aggregate is

   --------------------------------------------
   --  Storage for projects being aggregated --
   --------------------------------------------

   type Project_Record is record
      Project_Path : GPR2.Path_Name.Object := GPR2.Path_Name.Undefined;
      --  Full path to a project file
   end record;

   function "=" (L, R : Project_Record) return Boolean;
   function "<" (L, R : Project_Record) return Boolean;
   --  These two functions compare project paths

   package Aggregated_Projects_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Project_Record);
   use Aggregated_Projects_Sets;

   Aggregated_Projects : Set;

   --  Project iterator - old-fashion manual iteration through the ordered set
   --  is used.
   Iterator_Done   : Boolean := True;
   Iterator_C      : Cursor  := No_Element;
   Iterator_El     : Project_Record;

   Aggregated_Prj_Name : String_Access;
   --  The name of a project file passed as a parameter of '-A' option

   ------------------------
   --  Local subprograms --
   ------------------------

   -------------
   -- "=" "<" --
   -------------

   function "=" (L, R : Project_Record) return Boolean is
      use GPR2.Path_Name;
   begin
      if R.Project_Path = Undefined and then R.Project_Path = Undefined then
         return True;
      elsif R.Project_Path = Undefined or else R.Project_Path = Undefined then
         return False;
      else
         return L.Project_Path.Value = R.Project_Path.Value;
      end if;
   end "=";

   function "<" (L, R : Project_Record) return Boolean is
      use GPR2.Path_Name;
   begin
      if L = R then
         return False;
      elsif L.Project_Path = Undefined then
         return True;
      elsif R.Project_Path = Undefined then
         return False;
      else
         return L.Project_Path.Value < R.Project_Path.Value;
      end if;
   end "<";

   ---------------------------------
   -- Collect_Aggregated_Projects --
   ---------------------------------

   procedure Collect_Aggregated_Projects (P : GPR2.Project.View.Object) is
      New_Prj_Rec     : Project_Record;
   begin
      for Agg of P.Aggregated loop
         if Agg.Kind in GPR2.Aggregate_Kind then
            --  Unwind nested aggregate projects
            Collect_Aggregated_Projects (Agg);
         else
            New_Prj_Rec.Project_Path := Agg.Path_Name;
            Include (Aggregated_Projects, New_Prj_Rec);
         end if;
      end loop;
   end Collect_Aggregated_Projects;

   ----------------------------
   -- Get_Aggregated_Prj_Src --
   ----------------------------

   function Get_Aggregated_Prj_Src return GPR2.Path_Name.Object is
     (Aggregated_Projects.First_Element.Project_Path);

   ----------------------------
   -- Get_Aggregated_Project --
   ----------------------------

   function Get_Aggregated_Project return String is (Aggregated_Prj_Name.all);

   -------------------
   -- Next_Prj_Name --
   -------------------

   function Next_Prj_Name return GPR2.Path_Name.Full_Name is
   begin
      if not Iterator_Done then
         return Iterator_El.Project_Path.Value;
      else
         Error ("attempt to get project name for non-active iterator");
         raise Fatal_Error;
      end if;
   end Next_Prj_Name;

   --------------------------------
   -- Num_Of_Aggregated_Projects --
   --------------------------------

   function Num_Of_Aggregated_Projects return Natural is
     (Natural (Aggregated_Projects.Length));

   -----------------------
   -- Prj_Iterator_Done --
   -----------------------

   function Prj_Iterator_Done return Boolean is (Iterator_Done);

   -----------------------
   -- Prj_Iterator_Next --
   -----------------------

   procedure Prj_Iterator_Next is
   begin
      Iterator_C := Next (Iterator_C);

      if not Has_Element (Iterator_C) then
         Iterator_Done := True;
      else
         Iterator_El := Element (Iterator_C);
      end if;

   end Prj_Iterator_Next;

   ---------------------------------
   -- Process_Aggregated_Projects --
   ---------------------------------

   procedure Process_Aggregated_Projects
     (My_Project : Arg_Project_Type'Class)
   is
      Report_File_Name     : constant String := (if Text_Report_ON then
                                                     Get_Report_File_Name
                                                 else
                                                     "");
      XML_Report_File_Name : constant String := (if XML_Report_ON then
                                                    Get_XML_Report_File_Name
                                                 else
                                                    "");

      Count : Natural := 1;
      --  Counts iterations on aggregated projects, this number is used to
      --  create a unique name of the report files for each iteration

      Prj_Out_File   : constant String  := Report_File_Name;
      Prj_Out_First  : constant Natural := Prj_Out_File'First;
      Prj_Out_Last   : constant Natural := Prj_Out_File'Last;
      Prj_Out_Dot    :          Natural := Index (Prj_Out_File, ".", Backward);
      Prj_Out_Suffix : constant String :=
        (if Prj_Out_Dot = 0 then ""
         else Prj_Out_File (Prj_Out_Dot .. Prj_Out_Last));

      Prj_XML_File   : constant String  := XML_Report_File_Name;
      Prj_XML_First  : constant Natural := Prj_XML_File'First;
      Prj_XML_Last   : constant Natural := Prj_XML_File'Last;
      Prj_XML_Dot    :          Natural := Index (Prj_XML_File, ".", Backward);
      Prj_XML_Suffix : constant String :=
        (if Prj_XML_Dot = 0 then ""
         else Prj_XML_File (Prj_XML_Dot .. Prj_XML_Last));

      Prj_Args       : Argument_List (1 .. 2);
      Out_Args       : Argument_List (1 .. 4);
      Out_Args_Count : constant Integer :=
        (if Text_Report_ON and then XML_Report_ON then 4 else 2);

      Args      : Argument_List (1 .. Argument_Count);
      Arg_Count : Natural := 0;
      Skip_Next : Boolean := False;

      Exit_Code : Integer;

      Full_Tool_Name : constant String_Access :=
        Locate_Exec_On_Path (Executable);

   begin
      if Full_Tool_Name = null then
         Error ("Cannot locate " & Executable &
                " on PATH, possible installation problem");
         raise Fatal_Error;
      end if;

      if Prj_Out_Dot = 0 then
         Prj_Out_Dot := Prj_Out_Last;
      else
         Prj_Out_Dot := Prj_Out_Dot - 1;
      end if;

      if Prj_XML_Dot = 0 then
         Prj_XML_Dot := Prj_XML_Last;
      else
         Prj_XML_Dot := Prj_XML_Dot - 1;
      end if;

      Prj_Args (1) := new String'("-A");
      Out_Args (1) := new String'(if Text_Report_ON then "-o" else "-ox");

      if Out_Args_Count = 4 then
         Out_Args (3) := new String'("-ox");
      end if;

      for J in 1 .. Argument_Count loop
         declare
            Arg : constant String := Argument (J);
         begin
            if Skip_Next then
               Skip_Next := False;
            else
               --  Ignore -o/-ox switches

               if Arg = "-o" or else Arg = "-ox" then
                  Skip_Next := True;
               elsif Index (Arg, "-o=", Forward) = 0
                 and then Index (Arg, "-ox=", Forward) = 0
               then
                  Arg_Count := @ + 1;
                  Args (Arg_Count) := new String'(Arg);
               end if;
            end if;
         end;
      end loop;

      Aggregate_Project_Report_Header (My_Project);
      Start_Prj_Iterator;

      while not Prj_Iterator_Done loop
         if Verbose_Mode then
            Info ("Processing aggregated project " &
                  String (Next_Prj_Name));
         end if;

         Free (Prj_Args (2));
         Prj_Args (2) := new String'(String (Next_Prj_Name));

         Free (Out_Args (2));

         Out_Args (2) := new String'
           ((if Text_Report_ON then
                Prj_Out_File (Prj_Out_First .. Prj_Out_Dot) & "_" &
                Image (Count) & Prj_Out_Suffix
             else
                Prj_XML_File (Prj_XML_First .. Prj_XML_Dot) & "_" &
                Image (Count) & Prj_XML_Suffix));

         if Out_Args_Count = 4 then
            Out_Args (4) := new String'
                (Prj_XML_File (Prj_XML_First .. Prj_XML_Dot) & "_" &
                 Image (Count) & Prj_XML_Suffix);
         end if;

         Report_Aggregated_Project
           (Aggregate_Prj          => My_Project,
            Aggregated_Prj_Name    => Prj_Args (2).all,
            Expected_Text_Out_File => (if Text_Report_ON then
                                          Out_Args (2).all
                                       else
                                          ""),
            Expected_XML_Out_File  => (if XML_Report_ON then
                                          (if Out_Args_Count = 2 then
                                              Out_Args (2).all
                                          else
                                              Out_Args (4).all)
                                       else
                                          ""));

         if Debug_Mode then
            Put (Full_Tool_Name.all);

            for Arg of Prj_Args loop
               Put (" " & Arg.all);
            end loop;

            for Arg of Out_Args (1 .. Out_Args_Count) loop
               Put (" " & Arg.all);
            end loop;

            for Arg of Args (1 .. Arg_Count) loop
               Put (" " & Arg.all);
            end loop;

            New_Line;
         end if;

         Exit_Code := Spawn (Program_Name => Full_Tool_Name.all,
                             Args         => Prj_Args                       &
                                             Out_Args (1 .. Out_Args_Count) &
                                             Args (1 .. Arg_Count));

         Report_Aggregated_Project_Exit_Code
           (Aggregate_Prj       => My_Project,
            Exit_Code           => Exit_Code);

         Prj_Iterator_Next;
         Count := Count + 1;
      end loop;

      Close_Aggregate_Project_Report (My_Project);
   end Process_Aggregated_Projects;

   ------------------------
   -- Start_Prj_Iterator --
   ------------------------

   procedure Start_Prj_Iterator is
   begin
      Iterator_Done   := False;
      Iterator_C      := First (Aggregated_Projects);
      Iterator_El     := Element (Iterator_C);
   exception
      when others =>
         Error ("Cannot start iterator on aggregated projects");
         raise Fatal_Error;
   end Start_Prj_Iterator;

   ------------------------------
   -- Store_Aggregated_Project --
   ------------------------------

   procedure Store_Aggregated_Project (S : String) is
   begin
      Free (Aggregated_Prj_Name);
      Aggregated_Prj_Name := new String'(S);
   end Store_Aggregated_Project;

end Gnatcheck.Projects.Aggregate;
