with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;

package String_Utils is

   package String_Vectors is new
     Indefinite_Vectors (Positive, Unbounded_Text_Type);

   function Split_Lines (Str : Text_Type) return String_Vectors.Vector;
   --  Return a list of the lines in the given string

   function Underline_Range (Line  : Unbounded_Text_Type;
                             Start : Integer;
                             Stop  : Integer)
                             return  Unbounded_Text_Type;
   --  Underline the text in the given range. The input must contain a
   --  single line of text.

   function Underline_From (Line  : Unbounded_Text_Type;
                            Start : Integer)
                            return  Unbounded_Text_Type;
   --  Underline the text from the given index to the end of the line. The
   --  input must contain a single line of text.

   function Underline_To   (Line : Unbounded_Text_Type;
                            Stop : Integer)
                            return Unbounded_Text_Type;
   --  Underline the text from the begining of the line to the given index. The
   --  input must contain a single line of text.

   function Underline (Line : Unbounded_Text_Type) return Unbounded_Text_Type;
   --  Underline all the text in the input String. The input must contain a
   --  single line of text.

end String_Utils;
