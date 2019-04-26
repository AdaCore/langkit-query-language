with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Rule_Results is

   -----------------------
   -- To_Unbounded_Text --
   -----------------------

   function To_Unbounded_Text (Self : Rule_Result) return Unbounded_Text_Type
   is
      use Langkit_Support.Text.Chars;
   begin
      return Result : Unbounded_Text_Type do
         Result := Result & Self.Rule_Name;

         if Integer (Self.Messages.Length) = 0 then
            Result := Result & LF & HT & "No Match";
         else
            for Msg of Self.Messages loop
               Result := Result & LF & HT & Msg;
            end loop;
         end if;
      end return;
   end To_Unbounded_Text;

   -------------
   -- Display --
   -------------

   procedure Display (Self : Rule_Result) is
   begin
      Put_Line (Self.To_Unbounded_Text);
   end Display;

   ----------------------
   -- Make_Rule_Result --
   ----------------------

   function Make_Rule_Result (Rule_Name     : Unbounded_Text_Type;
                              Flagged_Nodes : Ada_Node_Vector)
                              return Rule_Result
   is
      Messages        : String_Vector;
   begin
      for N of Flagged_Nodes loop
         Messages.Append (To_Unbounded_Text (N.Text_Image));
      end loop;

      return Rule_Result'(Rule_Name, Messages);
   end Make_Rule_Result;

end Rule_Results;
