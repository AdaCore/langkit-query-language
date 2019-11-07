with Project_Utils; use Project_Utils;

package body Diagnostics is

   ---------------------------------
   -- Make_Diagnostic_For_Project --
   ---------------------------------

   function Make_Diagnostic_For_Project
     (Project_Path : String; Rules : Rule_Vectors.Vector) return Diagnostic
   is
   begin
      return Diagnostic'
        (To_Unbounded_Text (To_Text (Project_Path)), Rules);
   end Make_Diagnostic_For_Project;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Self : Diagnostic) return Result_Vector.Vector is
      Context  : LAL.Analysis_Context;
      Units    : constant Ada_Unit_Vector :=
        Load_Project (Self.Path_String, Context);
   begin
      return Result : Result_Vector.Vector do
         for Unit of Units loop
            for Rule of Self.Rules loop
               Result.Append (Rule.Evaluate (Units));
            end loop;
         end loop;
      end return;
   end Evaluate;

   -----------------
   -- Path_String --
   -----------------

   function Path_String (Self : Diagnostic) return String is
      (To_UTF8 (To_Text (Self.Project_Path)));

end Diagnostics;
