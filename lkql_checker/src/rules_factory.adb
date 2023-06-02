------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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
     (Ctx  : in out Eval_Context;
      Dirs : Path_Vector := Path_Vectors.Empty_Vector) return Rule_Vector
   is
      Rules_Dirs : constant Virtual_File_Array := Get_Rules_Directories (Dirs);
      Rules      : Rule_Vector := Rule_Vectors.Empty_Vector;
      Seen       : Rule_Set := Rule_Sets.Empty_Set;

   begin
      --  We search (non recursively) for all .lkql files in the Rules_Dir.
      --  Each file should contain the definition of one rule, and the needed
      --  helper functions.

      for Rules_Dir of Rules_Dirs loop
         if Is_Directory (Rules_Dir) then
            Ctx.Add_Lkql_Path (+Rules_Dir.Full_Name);

            declare
               Dir : File_Array_Access := Read_Dir (Rules_Dir);
            begin
               for File of Dir.all loop
                  if File.File_Extension = +".lkql"
                     and then not Seen.Contains (+File.Full_Name)
                  then
                     declare
                        Rc : Rule_Command;

                        Has_Rule : constant Boolean := Create_Rule_Command
                          (+File.Full_Name, Ctx, Rc);
                     begin
                        if Has_Rule then
                           Rules.Append (Rc);
                           Seen.Include (+File.Full_Name);
                        end if;
                     end;
                  end if;
               end loop;

               Unchecked_Free (Dir);
            end;
         end if;
      end loop;

      return Rules;
   end All_Rules;

   ---------------------------
   -- Get_Rules_Directories --
   ---------------------------

   function Get_Rules_Directories
     (Dirs : Path_Vector) return Virtual_File_Array
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

         Lkql : constant String := Compose (Compose (Prefix, "share"), "lkql");
         Kp   : constant String := Compose (Lkql, "kp");

         Builtin_Checkers_Dir : constant Virtual_File_Array :=
           [Create (+Lkql), Create (+Kp)];

         Custom_Checkers_Dirs : Virtual_File_Array
                                  (1 .. Integer (Dirs.Length));
         Index                : Positive := 1;

      begin
         for Dir of Dirs loop
            Custom_Checkers_Dirs (Index) := Create (+Dir);
            Index := @ + 1;
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
      null;
   end Finalize_Rules;

end Rules_Factory;
