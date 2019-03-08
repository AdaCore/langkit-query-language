with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

with GNAT.Case_Util;

package body Interpreter.Primitives is
   function Int_Image (Value : Integer) return Unbounded_Text_Type;
   --  Wraps the Integer'Wide_Wide_Image function, removing the leading space

   function Bool_Image (Value : Boolean) return Unbounded_Text_Type;
   --  Return a String representation of the given Boolean value

   function List_Image (Value : Primitive_List) return Unbounded_Text_Type;
   --  Return a String representation of the given Primitive_List value

   procedure Check_Kind (Expected_Kind : Primitive_Kind; Value : Primitive);
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

   -----------------
   --  Bool_Image --
   -----------------

   function Bool_Image (Value : Boolean) return Unbounded_Text_Type is
      use GNAT.Case_Util;
      Image : String := Boolean'Image (Value);
   begin
      To_Lower (Image);
      return To_Unbounded_Text (To_Text (Image));
   end Bool_Image;

   ----------------
   -- List_Image --
   ----------------

   function List_Image (Value : Primitive_List) return Unbounded_Text_Type is
      use Langkit_Support.Text.Chars;
      Image : Unbounded_Text_Type;
   begin
      for Element of Value.Elements loop
         Append (Image, To_Unbounded_Text (Element) & LF);
      end loop;

      return Image;
   end List_Image;

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind (Expected_Kind : Primitive_Kind; Value : Primitive) is
   begin
      if Value.Kind /= Expected_Kind then
         raise Unsupported_Error
           with "Expected " & Kind_Name (Value) & " but got " &
                To_String (Value.Kind);
      end if;
   end Check_Kind;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Integer) return Primitive is
   begin
      return (Kind => Kind_Int, Int_Val => Val);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Unbounded_Text_Type) return Primitive is
   begin
      return (Kind => Kind_Str, Str_Val => Val);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Boolean) return Primitive is
   begin
      return (Kind => Kind_Bool, Bool_Val => Val);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : LAL.Ada_Node) return Primitive is
   begin
      return (Kind => Kind_Node, Node_Val => Val);
   end To_Primitive;

   ---------------------
   -- Make_Empty_List --
   ---------------------

   function Make_Empty_List (Kind : Primitive_Kind) return Primitive is
      List : constant access Primitive_List :=
        new Primitive_List'(Elements_Kind => Kind,
                            Elements      => Primitive_Vectors.Empty_Vector);
   begin
      return (Kind     => Kind_List,
              List_Val => List);
   end Make_Empty_List;

   ------------
   -- Append --
   ------------

   procedure Append (List, Element : Primitive) is
   begin
      Check_Kind (Kind_List, List);
      Check_Kind (List.List_Val.Elements_Kind, Element);
      List.List_Val.Elements.Append (Element);
   end Append;

   --------------
   -- Contains --
   --------------

   function Contains (List, Value : Primitive) return Boolean is
   begin
      Check_Kind (Kind_List, List);
      Check_Kind (List.List_Val.Elements_Kind, Value);
      return List.List_Val.Elements.Contains (Value);
   end Contains;

   -----------------------
   -- To_Unbounded_Text --
   -----------------------

   function To_Unbounded_Text (Val : Primitive) return Unbounded_Text_Type is
   begin
      return (case Val.Kind is
                 when Kind_Unit => To_Unbounded_Text (To_Text ("()")),
                 when Kind_Int  => Int_Image (Val.Int_Val),
                 when Kind_Str  => Val.Str_Val,
                 when Kind_Bool => Bool_Image (Val.Bool_Val),
                 when Kind_Node => To_Unbounded_Text (Val.Node_Val.Text_Image),
                 when Kind_List => List_Image (Val.List_Val.all));
   end To_Unbounded_Text;

   ---------------
   -- To_String --
   ---------------

   function To_String (Val : Primitive_Kind) return String is
   begin
      return (case Val is
                 when Kind_Unit => "Unit",
                 when Kind_Int  => "Int",
                 when Kind_Str  => "Str",
                 when Kind_Bool => "Bool",
                 when Kind_Node => "Node",
                 when Kind_List => "List");
   end To_String;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Value : Primitive) return String is
   begin
      return (case Value.Kind is
                 when Kind_Unit =>
                   "Unit",
                 when Kind_Int =>
                   "Int",
                 when Kind_Str =>
                   "Str",
                 when Kind_Bool =>
                   "Bool",
                 when Kind_Node =>
                   LAL.Kind_Name (Value.Node_Val),
                 when Kind_List =>
                   To_String (Value.Kind) & '[' &
                   To_String (Value.List_Val.Elements_Kind) & ']');
   end Kind_Name;

   -------------
   -- Display --
   -------------

   procedure Display (Value : Primitive) is
   begin
      Put_Line (To_Unbounded_Text (Value));
   end Display;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Primitive) return Primitive is
   begin
      return (case Left.Kind is
                 when Kind_Int =>
                   Left.Int_Val + Right,
                 when Kind_Str =>
                   Left.Str_Val + Right,
                 when others =>
                    raise Unsupported_Error
                      with "Wrong left operand for '+': " &
                           Kind_Name (Left));
   end "+";

   ---------
   -- "+" --
   ---------

   function "+" (Left : Integer; Right : Primitive) return Primitive is
   begin
      case Right.Kind is
         when Kind_Int =>
            return To_Primitive (Left + Right.Int_Val);
         when others =>
            raise Unsupported_Error
              with "Cannot add a " & To_String (Right.Kind) & " to an Int";
      end case;
   end "+";

   ---------
   -- "+" --
   ---------

   function "+"
     (Left : Unbounded_Text_Type; Right : Primitive) return Primitive is
   begin
      return
        (case Right.Kind is
            when Kind_Int =>
              To_Primitive (Left & Int_Image (Right.Int_Val)),
            when Kind_Str =>
              To_Primitive (Left & Right.Str_Val),
            when Kind_Bool =>
              To_Primitive (Left & To_Unbounded_Text (Right)),
            when others =>
               raise Unsupported_Error
                 with "Cannot add a " & To_String (Right.Kind) & " to a Str");
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Left.Int_Val - Right.Int_Val);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Left.Int_Val * Right.Int_Val);
   end "*";

   --------
   -- "/"--
   --------

   function "/" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);

      if Right.Int_Val = 0 then
         raise Unsupported_Error with "Zero division";
      end if;

      return To_Primitive (Left.Int_Val / Right.Int_Val);
   end "/";

   -----------
   -- "and" --
   -----------

   function "and" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Bool, Left);
      Check_Kind (Kind_Bool, Right);
      return To_Primitive (Left.Bool_Val and Right.Bool_Val);
   end "and";

   ----------
   -- "or" --
   ----------

   function "or" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Bool, Left);
      Check_Kind (Kind_Bool, Right);
      return To_Primitive (Left.Bool_Val or Right.Bool_Val);
   end "or";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Primitive) return Primitive is
   begin
      if Left.Kind /= Right.Kind then
         raise Unsupported_Error
           with "Cannot check equality between a " &
                To_String (Left.Kind) & " and a " & To_String (Right.Kind);
      end if;

      return To_Primitive (Left = Right);
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/=" (Left, Right : Primitive) return Primitive is
      Eq : constant Primitive := Left = Right;
   begin
      return (Kind => Kind_Bool, Bool_Val => not Eq.Bool_Val);
   end "/=";

end Interpreter.Primitives;
