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

with Rule_Commands; use Rule_Commands;

with Liblkqllang.Analysis;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Rules_Factory is

   package L renames Liblkqllang.Analysis;

   package Rule_Vectors is new Ada.Containers.Vectors (Positive, Rule_Command);
   subtype Rule_Vector is Rule_Vectors.Vector;
   --  Vector of Rule_Command values

   type Path_Array is
     array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;
   Empty_Path_Array : constant Path_Array;

   function All_Rules
     (Ctx : Eval_Context; Dirs : Path_Array := Empty_Path_Array)
      return Rule_Vector;
   --  Return a vector containing Rule_Command values for every implemented
   --  check.

   procedure Finalize_Rules (Ctx : Eval_Context);
   --  Free memory associated to rules. Needs to be called at the end of the
   --  program.

private
   Empty_Path_Array : constant Path_Array := [];

   type Virtual_File_Array is array (Positive range <>) of Virtual_File;

   function Get_Rules_Directories
     (Dirs : Path_Array) return Virtual_File_Array;
   --  Return the absolute path of the directory containing the LKQL programs

end Rules_Factory;
