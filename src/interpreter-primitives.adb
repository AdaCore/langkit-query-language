with Ada.Assertions;                  use Ada.Assertions;
with Ada.Finalization;                use Ada.Finalization;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers;                  use type Ada.Containers.Count_Type;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

with GNAT.Case_Util;

package body Interpreter.Primitives is

   function Int_Image (Value : Integer) return Unbounded_Text_Type;
   --  Wraps the Integer'Wide_Wide_Image function, removing the leading space

   function Bool_Image (Value : Boolean) return Unbounded_Text_Type;
   --  Return a String representation of the given Boolean value

   function Iterator_Image
     (Value : Iterator_Primitive) return Unbounded_Text_Type;

   function List_Image (Value : Primitive_List) return Unbounded_Text_Type;
   --  Return a String representation of the given Primitive_List value

   function Fun_Image (Value : L.Fun_Def) return Unbounded_Text_Type;
   --  Return a String for the form: name[arity] representing the given
   --  function declaration.

   function Selector_Image (Value : L.Selector_Def) return Unbounded_Text_Type;
   --  Return a String of te form: Selector[name] representing the given
   --  selector definition.

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Value : Primitive);
   --  Raise an Unsupporter_Error exception if Value.Kind is different than
   --  Expected_Kind.

   function List_Data (Value : Primitive_List_Access;
                       Member_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Primitive List.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

   function Str_Data
     (Value : Unbounded_Text_Type; Member_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Str value.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

   procedure Raise_Unsupported_Operation
     (Left, Right : Primitive; Name : String)
     with No_Return;
   --  Raise an Unsupported_Operation exception mentionning the kind of the
   --  operands as well as the name of the operation.

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

   ---------------
   -- Fun_Image --
   ---------------

   function Fun_Image (Value : L.Fun_Def) return Unbounded_Text_Type is
      Arity : constant Unbounded_Text_Type :=
        Int_Image (Value.F_Parameters.Children_Count);
      Name  : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Value.F_Name.Text);
   begin
      return Name & '[' & Arity & ']';
   end Fun_Image;

   --------------------
   -- Selector_Image --
   --------------------

   function Selector_Image (Value : L.Selector_Def) return Unbounded_Text_Type
   is
   begin
      return "Selector[" & To_Unbounded_Text (Value.F_Name.Text) & ']';
   end Selector_Image;

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

   ---------------------------------
   -- Raise_Unsupported_Operation --
   ---------------------------------

   procedure Raise_Unsupported_Operation
     (Left, Right : Primitive; Name : String)
   is
      Message : constant String :=
        "Unsupported operation: " & Kind_Name (Left) & ' ' & Name &
          Kind_Name (Right);
   begin
      raise Unsupported_Error with Message;
   end Raise_Unsupported_Operation;

   -------------
   -- Release --
   -------------

   procedure Release (Data : in out Primitive_Data) is
      procedure Free_Iterator_Primitive_Access is
        new Ada.Unchecked_Deallocation
          (Iterator_Primitive, Iterator_Primitive_Access);
   begin
      case Data.Kind is
         when Kind_List =>
            Free_Primitive_List (Data.List_Val);
         when Kind_Iterator =>
            Free_Iterator_Primitive_Access (Data.Iter_Val);
         when others =>
            null;
      end case;
   end Release;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Iterator_Primitive) is
   begin
      Primitive_Iters.Release_Access (Object.Iter);
   end Finalize;

   -------------
   -- To_List --
   -------------

   function To_List (Iter : Iterator_Primitive) return Primitive is
      Element   : Primitive;
      Result    : constant Primitive := Make_Empty_List;
   begin
      while Iter.Iter.Next (Element) loop
         Append (Result, Element);
      end loop;

      Iter.Iter.Release;
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

   -------------
   -- Fun_Val --
   -------------

   function Fun_Val (Value : Primitive) return L.Fun_Def is
     (Value.Get.Fun_Val);

   ------------------
   -- Selector_Val --
   ------------------

   function Selector_Val (Value : Primitive) return L.Selector_Def is
      (Value.Get.Selector_Val);

   --------------
   -- Iter_Val --
   --------------

   function Iter_Val (Value : Primitive) return Iterator_Primitive is
      Iter_Primitive    : constant Iterator_Primitive_Access :=
        Value.Get.Iter_Val;
      Wrapped_Iter_Copy : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'
          (Primitive_Iter'Class (Iter_Primitive.Iter.Clone));
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

   function List_Data (Value : Primitive_List_Access;
                           Member_Name : Text_Type) return Primitive
   is
   begin
      if Member_Name = "length" then
         return To_Primitive (Integer (Value.Elements.Length));
      else
         raise Unsupported_Error with
           "No property named " & To_UTF8 (Member_Name) &
           " on values of kind " & To_String (Kind_List);
      end if;
   end List_Data;

   ------------------
   -- Str_Property --
   ------------------

   function Str_Data
     (Value : Unbounded_Text_Type; Member_Name : Text_Type) return Primitive
   is
   begin
      if Member_Name = "length" then
         return To_Primitive (Length (Value));
      else
         raise Unsupported_Error with
           "No property named " & To_UTF8 (Member_Name) &
           " on values of kind " & To_String (Kind_Str);
      end if;
   end Str_Data;

   --------------
   -- Property --
   --------------

   function Data
     (Value : Primitive; Member_Name : Text_Type) return Primitive
   is
   begin
      return (case Kind (Value) is
                 when Kind_List =>
                   List_Data (List_Val (Value), Member_Name),
                 when Kind_Str =>
                   Str_Data (Str_Val (Value), Member_Name),
                 when others =>
                    raise Unsupported_Error with
                      "Cannot get property on value of kind "
                       & Kind_Name (Value));
   end Data;

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

   function To_Primitive (Val : Text_Type) return Primitive is
     (To_Primitive (To_Unbounded_Text (Val)));

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
        new Primitive_Iter'Class'(Primitive_Iter'Class (Val.Clone));
      Iter_Primitive : constant Iterator_Primitive_Access :=
        new Iterator_Primitive'(Controlled with Val_Copy);
   begin
      return Result : Primitive do
         Result.Set
           (Primitive_Data'
              (Refcounted with Kind_Iterator, Iter_Primitive));
      end return;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : L.Fun_Def) return Primitive is
   begin
      return Result : Primitive do
         Result.Set (Primitive_Data'(Refcounted with Kind_Fun, Val));
      end return;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : L.Selector_Def) return Primitive is
   begin
      return Result : Primitive do
         Result.Set (Primitive_Data'(Refcounted with Kind_Selector, Val));
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
                   List_Image (Val.Get.List_Val.all),
                 when Kind_Fun =>
                   Fun_Image (Val.Get.Fun_Val),
                 when Kind_Selector =>
                   Selector_Image (Selector_Val (Val)));
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
                 when Kind_List      => "List",
                 when Kind_Fun       => "Fun",
                 when Kind_Selector  => "Selector");
   end To_String;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Value : Primitive) return String is
   begin
      return (case Value.Get.Kind is
                 when Kind_Node =>
                   LAL.Kind_Name (Value.Get.Node_Val),
                 when others =>
                   To_String (Kind (Value)));
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
      (To_Primitive (Deep_Equals (Left, Right)));

   -----------------
   -- Deep_Equals --
   -----------------

   function Deep_Equals (Left, Right : Primitive) return Boolean is
   begin
      if Kind (Left) /= Kind (Right) then
         raise Unsupported_Error
           with "Cannot check equality between a " & Kind_Name (Left) &
                " and a " & Kind_Name (Right);
      end if;

      return (case Kind (Left) is
                 when Kind_List =>
                   Deep_Equals (List_Val (Left), List_Val (Right)),
                 when others =>
                   Left.Get = Right.Get);
   end Deep_Equals;

   -----------------
   -- Deep_Equals --
   -----------------

   function Deep_Equals (Left, Right : Primitive_List_Access) return Boolean is
   begin
      if Left.Elements.Length /= Right.Elements.Length then
         return False;
      end if;

      for I in Left.Elements.First_Index .. Left.Elements.Last_Index loop
         if not Bool_Val (Left.Elements (I) = Right.Elements (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Deep_Equals;

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

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Primitive) return Primitive is
   begin
      if Kind (Left) /= Kind (Right) then
         Raise_Unsupported_Operation (Left, Right, "<");
      end if;

      case Kind (Left) is
         when Kind_Int =>
            return To_Primitive (Int_Val (Left) < Int_Val (Right));
         when Kind_Str =>
            return To_Primitive (Str_Val (Left) < Str_Val (Right));
         when others =>
            Raise_Unsupported_Operation (Left, Right, "<");
      end case;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Primitive) return Primitive is
     (if Bool_Val (Left < Right) then To_Primitive (True) else Left = Right);

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Primitive) return Primitive is
     (To_Primitive
        (not (Bool_Val (Left < Right) or else Bool_Val (Left = Right))));

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Primitive) return Primitive is
      (To_Primitive (not Bool_Val (Left < Right)));

end Interpreter.Primitives;
