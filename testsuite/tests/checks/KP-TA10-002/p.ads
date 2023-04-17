with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;

package P is

   package File_Sets is
      new Ada.Containers.Ordered_Sets (Element_Type => Unbounded_String);

   type R is record
      Source_Files : File_Sets.Set;
   end record;

   B : Boolean;

   function It1 (Self : R) return
     File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class
   is (case B is when others => Self.Source_Files.Iterate);  --  FLAG

   function It2 (Self : R) return
     File_Sets.Set_Iterator_Interfaces.Reversible_Iterator'Class
   is (if B then raise Program_Error else Self.Source_Files.Iterate);  --  FLAG

end;
