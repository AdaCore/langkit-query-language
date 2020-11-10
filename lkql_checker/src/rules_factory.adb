with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;

with GNAT.OS_Lib;
with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Text; use Langkit_Support.Text;

package body Rules_Factory is

   ---------------
   -- All_Rules --
   ---------------

   function All_Rules return Rule_Vector is
      Rules_File_Path : constant String :=
        Compose (Get_Rules_Directory, "rules", "json");
      Rules_File      : constant Virtual_File := Create (+Rules_File_Path);
      Rules_Content   : constant XString := Rules_File.Read_File;
      Rules_JSON      : constant JSON_Array := Get
        (GNATCOLL.JSON.Read (Rules_Content.To_String));
      Result : Rule_Vector;
   begin
      declare
      begin
         for I in 1 .. Length (Rules_JSON) loop
            declare
               Rule        : constant JSON_Value := Get (Rules_JSON, I);
               Rule_Name   : constant String := Rule.Get ("name");
               Rule_Path   : constant String := Rule.Get ("path");
               Rule_Params : constant JSON_Array := Rule.Get ("params");
               Rule_Code   : XString;
            begin
               Rule_Code.Append ("result(");
               for J in 1 .. Length (Rule_Params) loop
                  Rule_Code.Append
                    (Get (Rule_Params, J).Get ("default").Write);
                  if J < Length (Rule_Params) then
                     Rule_Code.Append (", ");
                  end if;
               end loop;
               Rule_Code.Append (")");
               Result.Append
                 (Create_Rule_Command
                    (Name             => To_Text (Rule_Name),
                     LKQL_Script_Path => Get_Rule_Path (Rule_Path),
                     Code             => To_Text (Rule_Code.To_String)));
            end;
         end loop;
      end;

      return Result;

   end All_Rules;

   -------------------
   -- Get_Rule_Path --
   -------------------

   function Get_Rule_Path (Rule_Name : String) return String is
   begin
      declare
         Ret : constant String :=
           Compose (Get_Rules_Directory, Rule_Name);
      begin
         return Ret;
      end;
   end Get_Rule_Path;

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
