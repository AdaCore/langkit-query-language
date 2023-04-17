with Ada.Strings.Unbounded;
with Common;

package Pattern is

   use Common;

   type Object is tagged private;
   --  We introduce this type to make for the fact that GNAT.Regexp doesn't
   --  give acces to the original string used to compile.

   function Create (Pattern : Pattern_Type) return Object'Class;
   --  Creates a Pattern

   function Pattern (Self : Object) return Pattern_Type;
   --  Returns the raw string used to compile Self

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Pattern  : Unbounded_String;
   end record;

   function Pattern (Self : Object) return Pattern_Type is
     (Pattern_Type (To_String (Self.Pattern)));

end Pattern;
