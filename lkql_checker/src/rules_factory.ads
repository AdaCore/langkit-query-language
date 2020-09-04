with Rule_Commands; use Rule_Commands;

with Liblkqllang.Analysis;

with Ada.Containers.Vectors;

package Rules_Factory is

   package L renames Liblkqllang.Analysis;

   package Rule_Vectors is new Ada.Containers.Vectors (Positive, Rule_Command);
   subtype Rule_Vector is Rule_Vectors.Vector;
   --  Vector of Rule_Command values

   function All_Rules return Rule_Vector;
   --  Return a vector containing Rule_Command values for every implemented
   --  check.

private

   function Get_Rule_Path (Rule_Name : String) return String;
   --  Return a String of the form: SCRIPS_DIR/Rule_Name.lkql, where
   --  SCRIPT_DIR is the absolute path of th
   --  directory containing the LKQL programs.

   function Get_Rules_Directory return String;
   --  Return the absolute path of the directory containing the LKQL programs

end Rules_Factory;
