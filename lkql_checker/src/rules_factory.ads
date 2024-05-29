--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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

   type Path_Array is array (Positive range <>) of Unbounded_String;
   No_Paths : Path_Array (1 .. 0) := [others => <>];

   function All_Rules
     (Ctx  : L.Analysis_Context;
      Dirs : Path_Array := No_Paths) return Rule_Vector;
   --  Return a vector containing Rule_Command values for every implemented
   --  check.

end Rules_Factory;
