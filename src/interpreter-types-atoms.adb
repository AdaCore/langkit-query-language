with Ada.Strings;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded;

package body Interpreter.Types.Atoms is
   function Int_Image (Value : Integer) return Unbounded_Text_Type;
   --  Wraps the Integer'Wide_Wide_Image function, removing the leading space

   procedure Check_Kind (Expected_Kind : Atom_Kind; Value : Atom);
   --  Raise an Unsupporter_Error exception if Value.Kind is different than
   --  Expected_Kind.

   ---------------
   -- Int_Image --
   ---------------

   function Int_Image (Value : Integer) return Unbounded_Text_Type is
      Image : constant Text_Type := Integer'Wide_Wide_Image (Value);
   begin
      return To_Unbounded_Text (Image (2 .. Image'Last));
   end Int_Image;

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind (Expected_Kind : Atom_Kind; Value : Atom) is
   begin
      if Value.Kind /= Expected_Kind then
         raise Unsupported_Error
           with "Expected " & To_String (Expected_Kind) & " but got " &
                To_String (Value.Kind);
      end if;
   end Check_Kind;

   ---------------
   -- To_String --
   ---------------

   function To_String (Kind : Atom_Kind) return String is
   begin
      return (case Kind is
                 when Kind_Unit =>
                   "",
                 when Kind_Int =>
                   "Int",
                 when Kind_Str =>
                   "Str",
                 when Kind_Bool =>
                   "Bool");
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : Atom) return Unbounded_Text_Type is
   begin
      return (case Value.Kind is
                 when Kind_Unit =>
                   To_Unbounded_Text ("()"),
                 when Kind_Int =>
                   Int_Image (Value.Int_Val),
                 when Kind_Str =>
                   Value.Str_Val,
                 when Kind_Bool =>
                   To_Unbounded_Text
                      (if Value.Bool_Val then "true" else "false"));
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
      return (case Left.Kind is
                 when Kind_Int =>
                   Left.Int_Val + Right,
                 when Kind_Str =>
                   Left.Str_Val + Right,
                 when others =>
                    raise Unsupported_Error
                      with "Wrong left operand for '+': " &
                           To_String (Left.Kind));
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
            raise Unsupported_Error
              with "Cannot add a " & To_String (Right.Kind) & " to an Int";
      end case;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Unbounded_Text_Type; Right : Atom) return Atom is
   begin
      return
        (case Right.Kind is
            when Kind_Int =>
              (Kind    => Kind_Str,
               Str_Val => Left & Int_Image (Right.Int_Val)),
            when Kind_Str =>
              (Kind => Kind_Str, Str_Val => Left & Right.Str_Val),
            when Kind_Bool =>
              (Kind    => Kind_Str,
               Str_Val => Left & To_String (Right)),
            when others =>
               raise Unsupported_Error
                 with "Cannot add a " & To_String (Right.Kind) & " to a Str");
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Atom) return Atom is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return (Kind => Kind_Int, Int_Val => Left.Int_Val - Right.Int_Val);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Atom) return Atom is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return (Kind => Kind_Int, Int_Val => Left.Int_Val * Right.Int_Val);
   end "*";

   --------
   -- "/"--
   --------

   function "/" (Left, Right : Atom) return Atom is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);

      if Right.Int_Val = 0 then
         raise Unsupported_Error with "Zero division";
      end if;

      return (Kind => Kind_Int, Int_Val => Left.Int_Val / Right.Int_Val);
   end "/";

   ----------
   -- "/=" --
   ----------

   function "/=" (Left, Right : Atom) return Atom is
      Eq : constant Atom := Left = Right;
   begin
      return (Kind => Kind_Bool, Bool_Val => not Eq.Bool_Val);
   end "/=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Atom) return Atom is
   begin
      if Left.Kind /= Right.Kind then
         raise Unsupported_Error
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
      Check_Kind (Kind_Bool, Left);
      Check_Kind (Kind_Bool, Right);
      return (Kind => Kind_Bool, Bool_Val => Left.Bool_Val and Right.Bool_Val);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Atom) return Atom is
   begin
      Check_Kind (Kind_Bool, Left);
      Check_Kind (Kind_Bool, Right);
      return (Kind => Kind_Bool, Bool_Val => Left.Bool_Val or Right.Bool_Val);
   end "or";

end Interpreter.Types.Atoms;
