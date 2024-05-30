--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Liblkqllang.Analysis;

with Rule_Commands; use Rule_Commands;

package Rules_Factory is

   package L renames Liblkqllang.Analysis;

   package Rule_Vectors is new Ada.Containers.Vectors (Positive, Rule_Command);
   subtype Rule_Vector is Rule_Vectors.Vector;
   --  Vector of Rule_Command values

   package Rule_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash, "=", "=");
   subtype Rule_Set is Rule_Sets.Set;

   package Path_Vectors is new
     Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype Path_Vector is Path_Vectors.Vector;

   function All_Rules
     (Ctx  : L.Analysis_Context;
      Dirs : Path_Vector := Path_Vectors.Empty_Vector) return Rule_Vector;
   --  Return a vector containing Rule_Command values for every implemented
   --  check.

private
   type Virtual_File_Array is array (Positive range <>) of Virtual_File;

   function Get_Rules_Directories
     (Dirs : Path_Vector) return Virtual_File_Array;
   --  Return the absolute path of the directory containing the LKQL programs

end Rules_Factory;
