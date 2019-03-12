with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;

package String_Utils is

   package String_Vectors is new
     Indefinite_Vectors (Positive, Unbounded_Text_Type);

   function Split_Lines (Str : Text_Type) return String_Vectors.Vector;
   --  Return a list of the lines in the given string

   function Underline_Range (Line  : Unbounded_Text_Type;
                             Start : Positive;
                             Stop  : Positive) return Unbounded_Text_Type;
   --  Return Line plus a new line character and a sequence characters so that
   --  all non-whitespace columns between `Start` and `Stop` (included) are '^'
   --  and the other columns are spaces.
   --
   --  The input must contain a single line of text.

   function Underline_From (Line  : Unbounded_Text_Type;
                            Start : Positive) return Unbounded_Text_Type;
   --  Return Line plus a new line character and a sequence characters so that
   --  all non-whistapce columns from `Start` to the end of the line are
   --  '^' and the other columns are spaces.
   --
   --  The input must contain a single line of text.

   function Underline_To (Line : Unbounded_Text_Type;
                          Stop : Positive) return Unbounded_Text_Type;
   --  Return Line plus a new line character and a sequence characters so that
   --  all non-whistapce columns from the begining of the line to `Stop` are
   --  '^' and the other columns are spaces.
   --
   --  The input must contain a single line of text.

   function Underline (Line : Unbounded_Text_Type) return Unbounded_Text_Type;
   --  Underline all the text in the input String. The input must contain a
   --  single line of text.

end String_Utils;
