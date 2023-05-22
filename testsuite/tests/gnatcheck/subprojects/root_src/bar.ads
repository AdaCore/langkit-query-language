with Ada.Containers.Vectors;

package Bar is
   package Int_Vectors is new Ada.Containers.Vectors
     (Positive, Integer, "=");

   procedure Main (X : Integer);
end Bar;
