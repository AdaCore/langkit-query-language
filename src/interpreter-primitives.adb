with Libadalang.Introspection; use Libadalang.Introspection;

with Ada.Assertions;                  use Ada.Assertions;
with Ada.Finalization;                use Ada.Finalization;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

with GNAT.Case_Util;

package body Interpreter.Primitives is
   subtype Field_Index is Integer range -1 .. Integer'Last;
   --  Represents the index of an AST node's field, retrieved using the
   --  introspection API.
   --  -1 is used to represent invalid fields

   function Int_Image (Value : Integer) return Unbounded_Text_Type;
   --  Wraps the Integer'Wide_Wide_Image function, removing the leading space

   function Bool_Image (Value : Boolean) return Unbounded_Text_Type;
   --  Return a String representation of the given Boolean value

   function Iterator_Image
     (Value : Iterator_Primitive) return Unbounded_Text_Type;

   function List_Image (Value : Primitive_List) return Unbounded_Text_Type;
   --  Return a String representation of the given Primitive_List value

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Value : Primitive);
   --  Raise an Unsupporter_Error exception if Value.Kind is different than
   --  Expected_Kind.

   function Get_Field_Index
     (Name : Text_Type; Node : LAL.Ada_Node) return Field_Index;
   --  Return the index of 'Node's field named 'Name', or -1 if there is no
   --  field matching the given name.

   function List_Property (Value : Primitive_List_Access;
                           Property_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Property_Name' of the given
   --  Primitive List.
   --  Raise an Unsupported_Error if there is no property named
   --  'Property_Name'.

   function Str_Property
     (Value : Unbounded_Text_Type; Property_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Property_Name' of the given
   --  Str value.
   --  Raise an Unsupported_Error if there is no property named
   --  'Property_Name'.

   function Node_Property
     (Value : LAL.Ada_Node; Property_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Property_Name' of the given
   --  Node value.
   --  Raise an Unsupported_Error if there is no property named
   --  'Property_Name'.

   function Built_In_Node_Property
     (Value : LAL.Ada_Node; Property_Name : Text_Type) return Primitive;
   --  Return the value of the built-in node property named 'Property_Name'.
   --  Raise an Unsupported_Error if there is no property named
   --  'Property_Name'.

   function Langkit_Node_Property
     (Value : LAL.Ada_Node; Property_Name : Text_Type) return Primitive;
   --  Return the value of the Langkit node property named 'Property_Name'.
   --  Raise an Unsupported_Error if there is no property named
   --  'Property_Name'.

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

   --------------------
   -- Iterator_Image --
   --------------------

   function Iterator_Image
     (Value : Iterator_Primitive) return Unbounded_Text_Type is
   begin
      return List_Image (List_Val (To_List (Value)).all);
   end Iterator_Image;

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Value : Primitive) is
   begin
      if Kind (Value) /= Expected_Kind then
         raise Unsupported_Error
           with "Expected " & To_String (Expected_Kind) & " but got " &
                Kind_Name (Value);
      end if;
   end Check_Kind;

   ---------------------
   -- Get_Field_Index --
   ---------------------

   function Get_Field_Index
     (Name : Text_Type; Node : LAL.Ada_Node) return Field_Index
   is
      UTF8_Name : constant String := To_UTF8 (Name);
   begin
      for F of Fields (Node.Kind) loop
         if Field_Name (F) = UTF8_Name then
            return Index (Node.Kind, F);
         end if;
      end loop;

      return -1;
   end Get_Field_Index;

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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Iterator_Primitive) is
   begin
      Free_Primitive_Iter (Object.Iter);
   end Finalize;

   -------------
   -- To_List --
   -------------

   function To_List (Iter : Iterator_Primitive) return Primitive is
      Element   : Primitive;
      Iter_Copy : Primitive_Iter'Class := Iter.Iter.all;
      Result    : constant Primitive := Make_Empty_List;
   begin
      while Iter_Copy.Next (Element) loop
         Append (Result, Element);
      end loop;

      return Result;
   end To_List;

   -----------------
   -- To_Iterator --
   -----------------

   function To_Iterator (Value : Primitive) return Primitive_Iter'Class is
     (case Value.Get.Kind is
      when Kind_Iterator =>
         Primitive_Iter'Class (Iter_Val (Value).Iter.Clone),
      when Kind_List     =>
         Primitive_Vec_Iters.To_Iterator (Elements (Value).all),
      when others =>
         raise Assertion_Error with
           "Cannot get an iterator from a value of kind : " &
           Kind_Name (Value));

   ----------
   -- Kind --
   ----------

   function Kind (Value : Primitive) return Valid_Primitive_Kind is
      (Value.Get.Kind);

   -------------
   -- Int_Val --
   -------------

   function Int_Val (Value : Primitive) return Integer is
      (Value.Get.Int_Val);

   -------------
   -- Str_Val --
   -------------

   function Str_Val (Value : Primitive) return Unbounded_Text_Type is
      (Value.Get.Str_Val);

   --------------
   -- Bool_Val --
   --------------

   function Bool_Val (Value : Primitive) return Boolean is
      (Value.Get.Bool_Val);

   --------------
   -- Node_Val --
   --------------

   function Node_Val (Value : Primitive) return LAL.Ada_Node is
      (Value.Get.Node_Val);

   --------------
   -- List_Val --
   --------------

   function List_Val (Value : Primitive) return Primitive_List_Access is
      (Value.Get.List_Val);

   --------------
   -- Iter_Val --
   --------------

   function Iter_Val (Value : Primitive) return Iterator_Primitive is
      Iter_Primitive    : constant Iterator_Primitive_Access :=
        Value.Get.Iter_Val;
      Wrapped_Iter_Copy : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'(Iter_Primitive.Iter.all);
   begin
      return Iterator_Primitive'(Ada.Finalization.Controlled with
                                   Wrapped_Iter_Copy);
   end Iter_Val;

   --------------
   -- Elements --
   --------------

   function Elements
     (Value : Primitive) return not null Primitive_Vector_Access is
   begin
      return Value.Get.List_Val.Elements'Access;
   end Elements;

   -------------------
   -- List_Property --
   -------------------

   function List_Property (Value : Primitive_List_Access;
                           Property_Name : Text_Type) return Primitive
   is
   begin
      if Property_Name = "length" then
         return To_Primitive (Integer (Value.Elements.Length));
      else
         raise Unsupported_Error with
           "No property named " & To_UTF8 (Property_Name) &
           " on values of kind " & To_String (Kind_List);
      end if;
   end List_Property;

   ------------------
   -- Str_Property --
   ------------------

   function Str_Property
     (Value : Unbounded_Text_Type; Property_Name : Text_Type) return Primitive
   is
   begin
      if Property_Name = "length" then
         return To_Primitive (Length (Value));
      else
         raise Unsupported_Error with
           "No property named " & To_UTF8 (Property_Name) &
           " on values of kind " & To_String (Kind_Str);
      end if;
   end Str_Property;

   -------------------
   -- Node_Property --
   -------------------

   function Node_Property
     (Value : LAL.Ada_Node; Property_Name : Text_Type) return Primitive
   is
   begin
      return (if Property_Name = "image"
              then Built_In_Node_Property (Value, Property_Name)
              else Langkit_Node_Property (Value, Property_Name));
   end Node_Property;

   ----------------------------
   -- Build_In_Node_Property --
   ----------------------------

   function Built_In_Node_Property
     (Value : LAL.Ada_Node; Property_Name : Text_Type) return Primitive
   is
   begin
      if Property_Name = "image" then
         return To_Primitive (To_Unbounded_Text (Value.Text_Image));
      end if;

      raise Assertion_Error with
        "Invalid built-in node property: " & To_UTF8 (Property_Name);
   end Built_In_Node_Property;

   ---------------------------
   -- Langkit_Node_Property --
   ---------------------------

   function Langkit_Node_Property
     (Value : LAL.Ada_Node; Property_Name : Text_Type) return Primitive
   is
      Index : constant Field_Index :=
        Get_Field_Index (Property_Name, Value);
   begin
      if Index = -1 then
         raise Unsupported_Error with
           "No field named " & To_UTF8 (Property_Name) &
           " on values of kind " & To_String (Kind_Node);
      end if;

      return To_Primitive (Value.Children (Index));
   end Langkit_Node_Property;

   --------------
   -- Property --
   --------------

   function Property
     (Value : Primitive; Property_Name : Text_Type) return Primitive
   is
   begin
      return (case Kind (Value) is
                 when Kind_List =>
                   List_Property (List_Val (Value), Property_Name),
                 when Kind_Str =>
                   Str_Property (Str_Val (Value), Property_Name),
                 when Kind_Node =>
                   Node_Property (Node_Val (Value), Property_Name),
                 when others =>
                    raise Unsupported_Error with
                      "Values of kind " & Kind_Name (Value) &
                      " dont have properties");
   end Property;

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

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Primitive_Iter'Class) return Primitive is
      Val_Copy : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'(Val);
      Iter_Primitive : constant Iterator_Primitive_Access :=
        new Iterator_Primitive'(Controlled with Val_Copy);
   begin
      return Result : Primitive do
         Result.Set
           (Primitive_Data'
              (Refcounted with Kind_Iterator, Iter_Primitive));
      end return;
   end To_Primitive;

   ---------------------
   -- Make_Empty_List --
   ---------------------

   function Make_Empty_List return Primitive is
      Ref  : Primitive;
      List : constant Primitive_List_Access :=
        new Primitive_List'(Elements => Primitive_Vectors.Empty_Vector);
   begin
      Ref.Set (Primitive_Data'(Refcounted with Kind     => Kind_List,
                                               List_Val => List));
      return Ref;
   end Make_Empty_List;

   ------------
   -- Append --
   ------------

   procedure Append (List, Element : Primitive) is
      List_Elements : constant Primitive_Vector_Access :=
        Elements (List);
   begin
      Check_Kind (Kind_List, List);
      List_Elements.Append (Element);
   end Append;

   --------------
   -- Contains --
   --------------

   function Contains (List, Value : Primitive) return Boolean is
   begin
      Check_Kind (Kind_List, List);

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

   ---------
   -- Get --
   ---------

   function Get (List : Primitive; Index : Integer) return Primitive is
      Vec : Primitive_Vector_Access;
   begin
      Check_Kind (Kind_List, List);
      Vec := Elements (List);

      if Index not in Vec.First_Index .. Vec.Last_Index then
         raise Unsupported_Error
           with "Invalid index: " & Integer'Image (Index);
      end if;

      return Vec.Element (Positive (Index));
   end Get;

   ------------
   -- Length --
   ------------

   function Length (List : Primitive) return Natural is
   begin
      Check_Kind (Kind_List, List);
      return Natural (Elements (List).Length);
   end Length;

   -----------------------
   -- To_Unbounded_Text --
   -----------------------

   function To_Unbounded_Text (Val : Primitive) return Unbounded_Text_Type is
   begin
      return (case Kind (Val) is
                 when Kind_Unit =>
                   To_Unbounded_Text (To_Text ("()")),
                 when Kind_Int  =>
                   Int_Image (Int_Val (Val)),
                 when Kind_Str  =>
                   Str_Val (Val),
                 when Kind_Bool =>
                   Bool_Image (Bool_Val (Val)),
                 when Kind_Node =>
                   To_Unbounded_Text (Val.Get.Node_Val.Text_Image),
                 when Kind_Iterator =>
                   Iterator_Image (Val.Get.Iter_Val.all),
                 when Kind_List =>
                   List_Image (Val.Get.List_Val.all));
   end To_Unbounded_Text;

   ---------------
   -- To_String --
   ---------------

   function To_String (Val : Valid_Primitive_Kind) return String is
   begin
      return (case Val is
                 when Kind_Unit      => "Unit",
                 when Kind_Int       => "Int",
                 when Kind_Str       => "Str",
                 when Kind_Bool      => "Bool",
                 when Kind_Node      => "Node",
                 when Kind_Iterator  => "Iterator",
                 when Kind_List      => "List");
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
                 when Kind_Iterator =>
                   "Iterator",
                 when Kind_List =>
                   "List");
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
      return To_Primitive (Int_Val (Left) + Int_Val (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Int_Val (Left) - Int_Val (Right));
   end "-";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);
      return To_Primitive (Int_Val (Left) * Int_Val (Right));
   end "*";

   --------
   -- "/"--
   --------

   function "/" (Left, Right : Primitive) return Primitive is
   begin
      Check_Kind (Kind_Int, Left);
      Check_Kind (Kind_Int, Right);

      if Int_Val (Right) = 0 then
         raise Unsupported_Error with "Zero division";
      end if;

      return To_Primitive (Int_Val (Left) / Int_Val (Right));
   end "/";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Primitive) return Primitive is
   begin
      if Kind (Left) /= Kind (Right) then
         raise Unsupported_Error
           with "Cannot check equality between a " & Kind_Name (Left) &
                " and a " & Kind_Name (Right);
      end if;

      return To_Primitive (Left.Get = Right.Get);
   end "=";

   ----------
   -- "/=" --
   ----------

   function "/=" (Left, Right : Primitive) return Primitive is
      Eq : constant Primitive := Left = Right;
   begin
      return To_Primitive (not Bool_Val (Eq));
   end "/=";

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Primitive) return Primitive is
      Left_Str  : Unbounded_Text_Type;
      Right_Str : Unbounded_Text_Type;
   begin
      Check_Kind (Kind_Str, Left);
      Left_Str := Str_Val (Left);

      Right_Str := (case Kind (Right) is
                    when Kind_Int  => Int_Image (Int_Val (Right)),
                    when Kind_Str  => Str_Val (Right),
                    when Kind_Bool => Bool_Image (Bool_Val (Right)),
                    when others =>
                       raise Unsupported_Error with
                         "Cannot add a " & Kind_Name (Right) & " to a Str");

      return To_Primitive (Left_Str & Right_Str);
   end "&";

end Interpreter.Primitives;
