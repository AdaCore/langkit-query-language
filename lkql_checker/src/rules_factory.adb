with Ada.Command_Line;
with Ada.Directories; use Ada.Directories;

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Text; use Langkit_Support.Text;

package body Rules_Factory is

   -----------------------------
   -- Create_Deep_Inheritance --
   -----------------------------

   function Create_Deep_Inheritance (Depth : String := "1") return Rule_Command
   is
   begin
      return Create_Rule_Command
        (Name             => "Deep inheritance hierarchies",
         Code        => "result(" & To_Text (Depth) & ")",
         LKQL_Script_Path => Get_Rule_Path ("deepInheritance"));
   end Create_Deep_Inheritance;

   -----------------------------
   -- Create_Multiple_Entries --
   -----------------------------

   function Create_Multiple_Entries return Rule_Command is
     (Create_Rule_Command
        (Name              => "Multiple Entries in protected declaration",
         LKQL_Script_Path  => Get_Rule_Path ("multipleEntries")));

   ---------------
   -- All_Rules --
   ---------------

   function All_Rules return Rule_Vector is
      Rules_File_Path : String :=
        Compose (Get_Rules_Directory, "rules", "json");
      Rules_File : Virtual_File := Create (+Rules_File_Path);
      Rules_Content : XString := Rules_File.Read_File;
      Rules_JSON      : JSON_Array := Get
        (GNATCOLL.JSON.Read (Rules_Content.To_String));
      Result : Rule_Vector;
   begin
      declare
         begin
         for I in 1 .. Length (Rules_JSON) loop
            declare
               Rule : JSON_Value := Get (Rules_JSON, I);
               Rule_Name : String := Rule.Get ("name");
               Rule_Path : String := Rule.Get ("path");
               Rule_Params : JSON_Array := Rule.Get ("params");
               Rule_Code : XString;
            begin
               Rule_Code.Append ("result(");
               for J in 1 .. Length (Rule_Params) loop
                  Rule_Code.Append (Get (Rule_Params, J).Get ("default").Write);
                  if J < Length (Rule_Params) then
                     Rule_Code.Append (", ");
                  end if;
               end loop;
               Rule_Code.Append (")");
               Put_Line ("Rule code: " & Rule_Code.To_String);
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
      use Ada.Directories;
      Executable  : constant String := Ada.Command_Line.Command_Name;
      Root_Dir    : constant String :=
        Containing_Directory
          (Containing_Directory (Containing_Directory (Executable)));
   begin
      return Compose (Root_Dir, "lkql");
   end Get_Rules_Directory;

end Rules_Factory;
