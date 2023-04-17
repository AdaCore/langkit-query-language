with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Unbounded;

package Common is

   use Ada.Strings.Unbounded;

   type Verbosity_Level_Type is (None, Low, High);

   --
   --  String types
   --

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   subtype Value_Type is String;
   --  A basic string type (case-sensitive, may be empty)

   type Value_Case_Insens is new Value_Type;

   overriding function "=" (Left, Right : Value_Case_Insens) return Boolean is
     (Ada.Strings.Equal_Case_Insensitive (String (Left), String (Right)));

   overriding function "<" (Left, Right : Value_Case_Insens) return Boolean is
     (Ada.Strings.Less_Case_Insensitive (String (Left), String (Right)));

   function Str_Hash_Case_Insensitive
     (Key : Value_Case_Insens) return Ada.Containers.Hash_Type is
     (Ada.Strings.Hash_Case_Insensitive (String (Key)));

   subtype Pattern_Type is String
     with Dynamic_Predicate => Pattern_Type'Length > 0;

   subtype Language_Type is Value_Case_Insens;

   --
   --  Containers
   --

   package String_Vector is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   package String_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (String, "<", "=");

   package Language_Set is
     new Ada.Containers.Indefinite_Ordered_Sets (Language_Type, "<", "=");

   package Language_Vector is
     new Ada.Containers.Indefinite_Vectors (Positive, Language_Type);

   --
   --  Constants
   --

   No_String : constant String := "";

   Ada_Lang : constant Language_Type := "Ada";

end Common;
