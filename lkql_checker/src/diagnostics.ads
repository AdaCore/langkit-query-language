with Rule_Results;  use Rule_Results;
with Rules_Factory; use Rules_Factory;

with Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Vectors;

package Diagnostics is

   package LAL renames Libadalang.Analysis;

   package Result_Vector is new Ada.Containers.Vectors (Positive, Rule_Result);

   type Diagnostic is tagged record
      Project_Path : Unbounded_Text_Type;
      --  Path of the GPR project file
      Rules        : Rules_Factory.Rule_Vector;
      --  Rules to execute to produce the diagnostic report
   end record;
   --  Record representing a diagnstic

   function Evaluate (Self : Diagnostic) return Result_Vector.Vector;
   --  Return the resut of executing every diagnostic's rule

   function Path_String (Self : Diagnostic) return String;
   --  Return a String representing the path of the diagnostic's GPR file

   function Make_Diagnostic_For_Project
     (Project_Path : String; Rules : Rule_Vector) return Diagnostic;
   --  Create a Diagnostic value with the given project file path and rules

end Diagnostics;
