------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                        G N A T C H E C K . I D S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2023, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 2, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General Public License distributed with GNAT; see file  COPYING. If --
-- not,  write to the  Free Software Foundation,  51 Franklin Street, Fifth --
-- Floor, Boston, MA 02110-1301, USA.                                       --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Conversions; use Ada.Characters.Conversions;

package body Gnatcheck.Ids is

   ------------------
   -- Find_Rule_Id --
   ------------------

   function Find_Rule_Id (Rule_Name : String) return Rule_Id is
      Rule_Id_Text : constant Text_Type := To_Lower (To_Text (Rule_Name));
   begin
      return Find (All_Rule_Ids, Rule_Id_Text, True);
   end Find_Rule_Id;

   -----------------
   -- Get_Id_Text --
   -----------------

   function Get_Id_Text (Id : Rule_Id) return String is
      Rule_Id_Text : constant Text_Type :=
        Image (To_Symbol (All_Rule_Ids, Id));
   begin
      return To_String (Wide_Wide_String (Rule_Id_Text));
   end Get_Id_Text;

   -----------------------
   -- Is_Compiler_Check --
   -----------------------

   function Is_Compiler_Check (Id : Rule_Id) return Boolean is
   begin
      return Id = Restrictions_Id
        or else Id = Warnings_Id
        or else Id = Style_Checks_Id;
   end Is_Compiler_Check;

end Gnatcheck.Ids;
