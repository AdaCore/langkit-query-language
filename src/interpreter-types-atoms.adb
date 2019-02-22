with Ada.Strings;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded;

package body Interpreter.Types.Atoms is
   ---------------
   -- To_String --
   ---------------

   function To_String (Kind : Atom_Kind) return String is
   begin
      case Kind is
         when Kind_Unit =>
            return "";
         when Kind_Int =>
            return "Int";
         when Kind_Str =>
            return "Str";
         when Kind_Bool =>
            return "Bool";
      end case;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Atom) return Unbounded_Text_Type is
   begin
      case Value.Kind is
         when Kind_Unit =>
            return To_Unbounded_Text ("()");
         when Kind_Int =>
            return To_Unbounded_Text (Integer'Wide_Wide_Image (Value.Int_Val));
         when Kind_Str =>
            return Value.Str_Val;
         when Kind_Bool =>
            return To_Unbounded_Text
                (if Value.Bool_Val then "true" else "false");
      end case;
   end To_String;

   -------------
   -- Display --
   -------------

   procedure Display (Value : Atom) is
   begin
      Put_Line (To_String (Value));
   end Display;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Atom) return Atom is
   begin
      case Left.Kind is
         when Kind_Int =>
            return Left.Int_Val + Right;
         when Kind_Str =>
            return Left.Str_Val + Right;
         when others =>
            raise Unsupported with "Wrong left operand for '+': ()";
      end case;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Integer; Right : Atom) return Atom is
   begin
      case Right.Kind is
         when Kind_Int =>
            return (Kind => Kind_Int, Int_Val => Left + Right.Int_Val);
         when others =>
            raise Unsupported
              with "Cannot add a " & To_String (Right.Kind) & " to an Int";
      end case;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Unbounded_Text_Type; Right : Atom) return Atom is
   begin
      case Right.Kind is
         when Kind_Int =>
            return (Kind => Kind_Str,
               Str_Val   => Left & Integer'Wide_Wide_Image (Right.Int_Val));
         when Kind_Str =>
            return (Kind => Kind_Str, Str_Val => Left & Right.Str_Val);
         when Kind_Bool =>
            return (Kind => Kind_Str,
               Str_Val   => Left & Boolean'Wide_Wide_Image (Right.Bool_Val));
         when others =>
            raise Unsupported
              with "Cannot add a " & To_String (Right.Kind) & " to a Str";
      end case;
   end "+";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Atom) return Atom is
   begin
      if Left.Kind /= Right.Kind then
         raise Unsupported
           with "Cannot check equality between a " &
           To_String (Left.Kind) & " and a " & To_String (Right.Kind);
      end if;

      return (Kind => Kind_Bool, Bool_Val => Left = Right);
   end "=";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Atom) return Atom is
   begin
      Check_Both_Bool (Left, Right);
      return (Kind => Kind_Bool, Bool_Val => Left.Bool_Val and Right.Bool_Val);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Atom) return Atom is
   begin
      Check_Both_Bool (Left, Right);
      return (Kind => Kind_Bool, Bool_Val => Left.Bool_Val or Right.Bool_Val);
   end "or";

   ---------------------
   -- Check_Both_Bool --
   ---------------------

   procedure Check_Both_Bool (Left, Right : Atom) is
   begin
      if Left.Kind /= Kind_Bool then
         raise Unsupported
           with "Wrong left operand type for logic operation: " &
           To_String (Left.Kind);
      elsif Right.Kind /= Kind_Bool then
         raise Unsupported
           with "Wrong right operand type for logic operation" &
           To_String (Right.Kind);
      end if;
   end Check_Both_Bool;

end Interpreter.Types.Atoms;
