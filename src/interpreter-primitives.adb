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
      if Value.Get.Kind /= Expected_Kind then
         raise Unsupported_Error
           with "Expected " & To_String (Expected_Kind) & " but got " &
                To_String (Value.Get.Kind);
      end if;
   end Check_Kind;

   -------------
   -- Release --
   -------------

   procedure Release (Data : in out Primitive_Data) is
   begin
      if Data.Kind /= Kind_List then
         return;
      end if;

      Free_Primitive_List (Data.List_Val);
   end Release;

   -------------------------
   -- Make_Unit_Primitive --
   -------------------------

   function Make_Unit_Primitive return Primitive is
      Ref : Primitive;
   begin
      Ref.Set (Primitive_Data'(Refcounted with Kind => Kind_Unit));
      return Ref;
   end Make_Unit_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Integer) return Primitive is
      Ref : Primitive;
   begin
      Ref.Set
        (Primitive_Data'(Refcounted with Kind => Kind_Int, Int_Val => Val));
      return Ref;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Unbounded_Text_Type) return Primitive is
      Ref : Primitive;
   begin
      Ref.Set
        (Primitive_Data'(Refcounted with Kind => Kind_Str, Str_Val => Val));
      return Ref;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Boolean) return Primitive is
      Ref : Primitive;
   begin
      Ref.Set
        (Primitive_Data'(Refcounted with Kind => Kind_Bool, Bool_Val => Val));
      return Ref;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : LAL.Ada_Node) return Primitive is
      Ref : Primitive;
   begin
      Ref.Set
        (Primitive_Data'(Refcounted with Kind => Kind_Node, Node_Val => Val));
      return Ref;
   end To_Primitive;

   ---------------------
   -- Make_Empty_List --
   ---------------------

   function Make_Empty_List (Kind : Primitive_Kind) return Primitive is
      Ref  : Primitive;
      List : constant Primitive_List_Access :=
        new Primitive_List'(Elements_Kind => Kind,
                            Elements      => Primitive_Vectors.Empty_Vector);
   begin
      Ref.Set (Primitive_Data'(Refcounted with Kind     => Kind_List,
                                               List_Val => List));
      return Ref;
   end Make_Empty_List;

   ------------
   -- Append --
   ------------

   procedure Append (List, Element : Primitive) is
   begin
      Check_Kind (Kind_List, List);
      Check_Kind (List.Get.List_Val.Elements_Kind, Element);
      List.Get.List_Val.Elements.Append (Element);
   end Append;

   --------------
   -- Contains --
   --------------

   function Contains (List, Value : Primitive) return Boolean is
   begin
      Check_Kind (Kind_List, List);
      Check_Kind (List.Get.List_Val.Elements_Kind, Value);

      --  Since we're using smart pointers, the "=" function used by
      --  Vector.Contains checks referencial equality instead of structural
      --  equality. So the iteration "has" to be done manually.
      for Elem of List.Get.List_Val.Elements loop
         if Elem.Get = Value.Get then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   -----------------------
   -- To_Unbounded_Text --
   -----------------------

   function To_Unbounded_Text (Val : Primitive) return Unbounded_Text_Type is
   begin
      return (case Val.Get.Kind is
                 when Kind_Unit =>
                   To_Unbounded_Text (To_Text ("()")),
                 when Kind_Int  =>
                   Int_Image (Val.Get.Int_Val),
                 when Kind_Str  =>
                   Val.Get.Str_Val,
                 when Kind_Bool =>
                   Bool_Image (Val.Get.Bool_Val),
                 when Kind_Node =>
                   To_Unbounded_Text (Val.Get.Node_Val.Text_Image),
                 when Kind_List =>
                   List_Image (Val.Get.List_Val.all));
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
      return (case Value.Get.Kind is
                 when Kind_Unit =>
                   "Unit",
                 when Kind_Int =>
                   "Int",
                 when Kind_Str =>
                   "Str",
                 when Kind_Bool =>
                   "Bool",
                 when Kind_Node =>
                   LAL.Kind_Name (Value.Get.Node_Val),
                 when Kind_List =>
                   To_String (Value.Get.Kind) & '[' &
                   To_String (Value.Get.List_Val.Elements_Kind) & ']');
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
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Left.Get.Int_Val + Right.Get.Int_Val);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Left.Get.Int_Val - Right.Get.Int_Val);
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Left.Get.Int_Val * Right.Get.Int_Val);
   end "*";

   --------
   -- "/"--
   --------

   function "/" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);

      if Right.Get.Int_Val = 0 then
         raise Unsupported_Error with "Zero division";
      end if;

      return To_Primitive (Left.Get.Int_Val / Right.Get.Int_Val);
   end "/";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Primitive) return Primitive is
   begin
      if Left.Get.Kind /= Right.Get.Kind then
         raise Unsupported_Error
           with "Cannot check equality between a " &
             To_String (Left.Get.Kind) & " and a " &
             To_String (Right.Get.Kind);
      end if;

      return To_Primitive (Left.Get = Right.Get);
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/=" (Left, Right : Primitive) return Primitive is
      Eq : constant Primitive := Left = Right;
   begin
      return To_Primitive (not Eq.Get.Bool_Val);
   end "/=";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Primitive) return Primitive is
      Result : Unbounded_Text_Type;
   begin
      Check_Kind (Kind_Str, Left);

      case Right.Get.Kind is
         when Kind_Int =>
            Result := Left.Get.Str_Val & Int_Image (Right.Get.Int_Val);
         when Kind_Str =>
            Result := Left.Get.Str_Val & Right.Get.Str_Val;
         when Kind_Bool =>
            Result := Left.Get.Str_Val & Bool_Image (Right.Get.Bool_Val);
         when others =>
            raise Unsupported_Error
               with "Cannot add a " & To_String (Right.Get.Kind) &
                    " to a Str";
      end case;

      return To_Primitive (Result);
   end "&";

end Interpreter.Primitives;
