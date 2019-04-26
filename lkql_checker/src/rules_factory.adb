with Exec;                      use Exec;
with Interpreter.Primitives;    use Interpreter.Primitives;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Command_Line, Ada.Directories;

package body Rules_Factory is

   -----------------------------
   -- Create_Deep_Inheritance --
   -----------------------------

   function Create_Deep_Inheritance (Depth : String := "1") return Rule_Command
   is
      Arguments      : Environment_Map;
      Depth_Arg_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text ("n");
      Depth_Value    : constant Primitive := Eval_LKQL_Code (Depth);
   begin
      Check_Kind (Kind_Int, Kind (Depth_Value),
                  "Create_Deep_Inheritance.Depth");

      Arguments.Insert (Depth_Arg_Name, Depth_Value);

      return Create_Rule_Command
        (Name             => "Deep inheritance hierarchies",
         Arguments        => Arguments,
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
   begin
      return Result : Rule_Vector do
         Result.Append (Create_Deep_Inheritance);
         Result.Append (Create_Multiple_Entries);
      end return;
   end All_Rules;

   -------------------
   -- Get_Rule_Path --
   -------------------

   function Get_Rule_Path (Rule_Name : String) return String is
      use Ada.Directories;
   begin
      return Compose (Get_Rules_Directory, Rule_Name, "lkql");
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
