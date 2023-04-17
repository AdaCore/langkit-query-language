with Ada.Containers.Indefinite_Hashed_Maps;

with Common;
with Pattern;
with Pattern.Vector;

package Section is

   use Common;
   use Pattern;

   type Object is tagged private;
   --  Object to store per-section information (dirs, patterns...)

   Empty_Section : constant Object;

   package Language_Patterns_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Language_Type,
      Pattern.Vector.Object,
      Str_Hash_Case_Insensitive,
      "=",
      Pattern.Vector."=");
   --  For excluded patterns: use a map from Languages to Pattern vectors
   --  because we're not interested in the insertion order, just lookup.

   procedure Add_Excluded_Language_Pattern
     (Self     : in out Object;
      Language : Language_Type;
      Pattern  : Pattern_Type);
   --  Adds an excluded naming pattern to the section

   function Excluded_Patterns
     (Self : Object) return Language_Patterns_Map.Map;
   --  Returns the map from languages to excluded patterns, as defined in
   --  section Self.

private

   type Object is tagged record
      Excluded_Patterns : Language_Patterns_Map.Map;
   end record;
   --  Description of a section on the command-line.
   --  For non-excluded patterns, we use a vector instead of a map since we
   --  need to preserve the command-line order.

   Empty_Section : constant Object :=
                     (Excluded_Patterns => Language_Patterns_Map.Empty_Map);

   function Excluded_Patterns
     (Self : Object) return Language_Patterns_Map.Map
   is (Self.Excluded_Patterns);

end Section;
