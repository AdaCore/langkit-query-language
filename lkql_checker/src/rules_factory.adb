------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;

with GNAT.OS_Lib;

package body Rules_Factory is

   ---------------
   -- All_Rules --
   ---------------

   function All_Rules
     (Ctx : Eval_Context; Dirs : Path_Array := Empty_Path_Array)
      return Rule_Vector
   is
      Rules_Dirs : constant Virtual_File_Array := Get_Rules_Directories (Dirs);
      Result     : Rule_Vector;
   begin

      --  We search (non recursively) for all .lkql files in the Rules_Dir.
      --  Each file should contain the definition of one rule, and the needed
      --  helper functions.

      for Rules_Dir of Rules_Dirs loop
         for File of Read_Dir (Rules_Dir).all loop
            if File.File_Extension = +".lkql" then
               declare
                  Rc : Rule_Command;
                  Has_Rule : constant Boolean := Create_Rule_Command
                    (+File.Full_Name, Ctx, Rc);
               begin
                  if Has_Rule then
                     Result.Append (Rc);
                  end if;
               end;
            end if;
         end loop;
      end loop;

      return Result;

   end All_Rules;

   -------------------------
   -- Get_Rules_Directory --
   -------------------------

   function Get_Rules_Directories
     (Dirs : Path_Array) return Virtual_File_Array
   is
      use Ada;
      use GNAT.OS_Lib;

      --  Assuming this program is installed in $PREFIX/bin, this computes
      --  $PREFIX/share/lkql.

      Executable : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path
        (Ada.Command_Line.Command_Name);
   begin
      if Executable = null then
         raise Program_Error with
            "cannot locate " & Ada.Command_Line.Command_Name;
      end if;

      declare
         Prefix : constant String :=
            Containing_Directory (Containing_Directory (Executable.all));

         Builtin_Checkers_Dir : constant Virtual_File_Array :=
           (1 => Create (+Compose (Compose (Prefix, "share"), "lkql")));

         Custom_Checkers_Dirs : Virtual_File_Array (Dirs'Range);
      begin
         for I in Dirs'Range loop
            Custom_Checkers_Dirs (I) :=
               Create (+Ada.Strings.Unbounded.To_String (Dirs (I)));
         end loop;
         Free (Executable);
         return Builtin_Checkers_Dir & Custom_Checkers_Dirs;
      end;
   end Get_Rules_Directories;

   --------------------
   -- Finalize_Rules --
   --------------------

   procedure Finalize_Rules (Ctx : Eval_Context) is
   begin
      for Rule of All_Rules (Ctx) loop
         declare
            R : Rule_Command := Rule;
         begin
            Destroy (R);
         end;
      end loop;
   end Finalize_Rules;

end Rules_Factory;
