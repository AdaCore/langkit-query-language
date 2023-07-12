with Ada.Containers.Vectors;

package Foo is
   package Int_Vectors is new Ada.Containers.Vectors  --  FLAG
     (Positive, Integer, "=");

   procedure Main (X : Integer);
end Foo;
