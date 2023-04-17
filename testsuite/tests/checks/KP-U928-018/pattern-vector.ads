with Ada.Containers.Indefinite_Vectors;

package Pattern.Vector is

   package Vector is new Ada.Containers.Indefinite_Vectors
     (Positive, Pattern.Object);

   subtype Object is Vector.Vector;

   subtype Cursor is Vector.Cursor;

   function "=" (Left, Right : Object) return Boolean renames Vector."=";

end Pattern.Vector;
