--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Environment_Variables;

with GNAT.OS_Lib;

with GNATCOLL.Utils;

package body Rules_Factory is

   ---------------
   -- All_Rules --
   ---------------

   function All_Rules
     (Ctx  : L.Analysis_Context;
      Dirs : Path_Vector := Path_Vectors.Empty_Vector) return Rule_Vector
   is
      Rules_Dirs : constant Virtual_File_Array := Get_Rules_Directories (Dirs);
      Rules      : Rule_Vector := Rule_Vectors.Empty_Vector;
      Seen       : String_Sets.Set := String_Sets.Empty_Set;

   begin
      --  We search (non recursively) for all .lkql files in the Rules_Dir.
      --  Each file should contain the definition of one rule, and the needed
      --  helper functions.

      for Rules_Dir of Rules_Dirs loop
         if Is_Directory (Rules_Dir) then

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
      function Add_Rules_Path (Path : String) return Boolean;

      Lkql_Rules_Paths : Path_Vector;

      function Add_Rules_Path (Path : String) return Boolean is
      begin
         Lkql_Rules_Paths.Append (Path);
         return True;
      end Add_Rules_Path;
   begin
      if Ada.Environment_Variables.Exists ("LKQL_RULES_PATH") then
         GNATCOLL.Utils.Split
           (Ada.Environment_Variables.Value ("LKQL_RULES_PATH"),
            GNAT.OS_Lib.Path_Separator & "",
            Add_Rules_Path'Access);
      end if;

      declare
         Custom_Checkers_Dirs : Virtual_File_Array
           (1 .. Integer (Dirs.Length) + Integer (Lkql_Rules_Paths.Length));

         Index                : Positive := 1;
      begin
         for Dir of Dirs loop
            Custom_Checkers_Dirs (Index) := Create (+Dir);
            Index := @ + 1;
         end loop;

         for Dir of Lkql_Rules_Paths loop
            Custom_Checkers_Dirs (Index) := Create (+Dir);
            Index := @ + 1;
         end loop;

         return Custom_Checkers_Dirs;
      end;
   end Get_Rules_Directories;

end Rules_Factory;
