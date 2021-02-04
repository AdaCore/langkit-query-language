with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;

with GNAT.OS_Lib;
with GNATCOLL.VFS; use GNATCOLL.VFS;

package body Rules_Factory is

   ---------------
   -- All_Rules --
   ---------------

   function All_Rules return Rule_Vector is
      Rules_Dir       : constant Virtual_File := Create (+Get_Rules_Directory);
      Rules_Dir_Files : constant File_Array_Access := Read_Dir (Rules_Dir);
      Result          : Rule_Vector;
   begin

      --  We search (non recursively) for all .lkql files in the Rules_Dir.
      --  Each file should contain the definition of one rule, and the needed
      --  helper functions.

      for File of Rules_Dir_Files.all loop
         if File.File_Extension = +".lkql" then
            Result.Append (Create_Rule_Command (+File.Full_Name));
         end if;
      end loop;

      return Result;

   end All_Rules;

   -------------------------
   -- Get_Rules_Directory --
   -------------------------

   function Get_Rules_Directory return String is
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
      begin
         Free (Executable);
         return Compose (Compose (Prefix, "share"), "lkql");
      end;
   end Get_Rules_Directory;

end Rules_Factory;
