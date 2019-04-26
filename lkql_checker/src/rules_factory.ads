with Rule_Commands; use Rule_Commands;

with Liblkqllang.Analysis;

with Ada.Containers.Vectors;

package Rules_Factory is

   package L renames Liblkqllang.Analysis;

   package Rule_Vectors is new Ada.Containers.Vectors (Positive, Rule_Command);
   subtype Rule_Vector is Rule_Vectors.Vector;
   --  Vector of Rule_Command values

   function Create_Deep_Inheritance
     (Depth : String := "1") return Rule_Command;
   --  Create a Rule_Command value that runs a check based on GNATCheck's
   --  predifined rule 'Deep inheritance hierarchies' (section 9.1.2.2).
   --  'Depth' corresponds to the maximum allowed depth of an inheritance
   --  hierarchy and must be a String representing an LKQL Integer.

   function Create_Multiple_Entries return Rule_Command;
   --  Create a Rule_Command value that runs a check based on GNATCheck's
   --  predifined rule 'Multiple entries in protected definitions'
   --  (section 9.1.2.2).

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
