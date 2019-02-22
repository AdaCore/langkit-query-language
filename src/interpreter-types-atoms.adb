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
         when Kind_Number =>
            return "Number";
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
         when Kind_Number =>
            return To_Unbounded_Text (Format_Float (Value.Number_Val));
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

   ------------------
   -- Format_Float --
   ------------------

   function Format_Float (N : Float) return Text_Type is
      Integral     : constant Integer   := Integer (N);
      Integral_Str : constant Text_Type := Integer'Wide_Wide_Image (Integral);
      Decimal      : constant Float     := N - Float (Integral);
      Decimal_Int  : constant Integer   :=
        Integer (Decimal * (Float (10**Float'Exponent (Decimal))));
      Decimal_Str : constant Text_Type :=
        Integer'Wide_Wide_Image (Decimal_Int);
      Repr : constant Text_Type :=
        Integral_Str (Integral_Str'First + 1 .. Integral_Str'Last) & "." &
        Decimal_Str (Decimal_Str'First + 1 .. Decimal_Str'Last);
   begin
      return
        (if N > 0.0001 and then N < 9_999_999_999.0 then Repr
         else Float'Wide_Wide_Image (N));
   end Format_Float;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Atom) return Atom is
   begin
      case Left.Kind is
         when Kind_Number =>
            return Left.Number_Val + Right;
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

   function "+" (Left : Float; Right : Atom) return Atom is
   begin
      case Right.Kind is
         when Kind_Number =>
            return (Kind  => Kind_Number,
               Number_Val => Left + Right.Number_Val);
         when Kind_Int =>
            return (Kind  => Kind_Number,
               Number_Val => Left + Float (Right.Int_Val));
         when others =>
            raise Unsupported
              with "Cannot add a " & To_String (Right.Kind) & " to a Float";
      end case;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Integer; Right : Atom) return Atom is
   begin
      case Right.Kind is
         when Kind_Number =>
            return (Kind => Kind_Int, Int_Val => Left + Right.Int_Val);
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
         when Kind_Number =>
            return (Kind => Kind_Str,
               Str_Val   => Left & Float'Wide_Wide_Image (Right.Number_Val));
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
           with "Cannot check for equality between a " &
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
