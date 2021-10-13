------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Assertions;                  use Ada.Assertions;
with Ada.Containers;                  use type Ada.Containers.Count_Type;
with Ada.Containers.Generic_Array_Sort;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Text_IO;

with LKQL.AST_Nodes;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;
with LKQL.Errors;         use LKQL.Errors;

with GNAT.Case_Util;

package body LKQL.Primitives is

   function Int_Image (Value : Adaptive_Integer) return Unbounded_Text_Type;
   --  Wraps the Integer'Wide_Wide_Image function, removing the leading space

   function Bool_Image (Value : Boolean) return Unbounded_Text_Type;
   --  Return a String representation of the given Boolean value

   function Iterator_Image
     (Value : Iterator_Primitive) return Unbounded_Text_Type;

   function Selector_List_Image
     (Value : Selector_List) return Unbounded_Text_Type;
   --  Return a String representation of the given Selector_List

   function List_Image (Value : Primitive_List;
                        Open  : Text_Type := "[";
                        Close : Text_Type := "]") return Unbounded_Text_Type;
   --  Return a String representation of the given Primitive_List value

   function Object_Image (Value : Primitive_Assocs) return Unbounded_Text_Type;
   --  Given a ``Primitive_Assocs``, return a textual representation as ``{key:
   --  <val>, ...}``.

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Value : Primitive);
   --  Raise an Unsupporter_Error exception if Value.Kind is different than
   --  Expected_Kind.

   function Selector_List_Data (Value       : Selector_List;
                                Member_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Primitive Selector_List.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

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

   function Iterator_Data (Value : Iterator_Primitive_Access;
                           Member_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Iterator value.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

   procedure Raise_Unsupported_Operation
     (Left, Right : Primitive; Name : String)
     with No_Return;
   --  Raise an Unsupported_Operation exception mentionning the kind of the
   --  operands as well as the name of the operation.

   package Primitive_Pool_Stacks
   is new Ada.Containers.Vectors (Positive, Primitive_Pool);

   Pool_Stack : Primitive_Pool_Stacks.Vector;

   function Pool return Primitive_Pool
   is
     (Pool_Stack.Last_Element);

   ---------------
   -- Int_Image --
   ---------------

   function Int_Image (Value : Adaptive_Integer) return Unbounded_Text_Type is
      Image : constant Text_Type := To_Text (Adaptive_Integers.Image (Value));
   begin
      if Image (1) = ' ' then
         return To_Unbounded_Text (Image (2 .. Image'Last));
      else
         return To_Unbounded_Text (Image);
      end if;
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

   -------------------------
   -- Selector_List_Image --
   -------------------------

   function Selector_List_Image
     (Value : Selector_List) return Unbounded_Text_Type
   is
      use Langkit_Support.Text.Chars;
      Image   : Unbounded_Text_Type;
   begin
      for D of Value.Depth_Nodes loop
         Append (Image, Text_Type'(D.Node.Unchecked_Get.Text_Image) & LF);
      end loop;

      return Image;
   end Selector_List_Image;

   ----------------
   -- List_Image --
   ----------------

   function List_Image (Value : Primitive_List;
                        Open  : Text_Type := "[";
                        Close : Text_Type := "]") return Unbounded_Text_Type
   is
      Image : Unbounded_Text_Type;
   begin
      Append (Image, Open);

      for I in Value.Elements.First_Index .. Value.Elements.Last_Index loop
         Append (Image, To_Unbounded_Text (Value.Elements (I)));

         if I < Value.Elements.Last_Index then
            Append (Image, ", ");
         end if;
      end loop;

      Append (Image, Close);

      return Image;
   end List_Image;

   ------------------
   -- Object_Image --
   ------------------

   function Object_Image (Value : Primitive_Assocs) return Unbounded_Text_Type
   is
      Image : Unbounded_Text_Type;
      use Primitive_Maps;
   begin
      Append (Image, "{");

      declare
         type Cursor_Array
         is array (Positive range <>) of Primitive_Maps.Cursor;

         Cursors : Cursor_Array (1 .. Natural (Value.Elements.Length));

         function "<" (L, R : Primitive_Maps.Cursor) return Boolean
         is
            (Key (L).all < Key (R).all);

         procedure Cursor_Sort
         is new Ada.Containers.Generic_Array_Sort
           (Positive, Primitive_Maps.Cursor, Cursor_Array, "<");

         I : Positive := 1;
      begin
         for Cur in Value.Elements.Iterate loop
            Cursors (I) := Cur;
            I := I + 1;
         end loop;

         Cursor_Sort (Cursors);

         I := 1;

         for Cur of Cursors loop
            Append (Image, """" & Key (Cur).all & """");
            Append (Image, ": ");
            Append (Image, To_Unbounded_Text (Primitive_Maps.Element (Cur)));
            if I < Positive (Value.Elements.Length) then
               Append (Image, ", ");
            end if;
            I := I + 1;
         end loop;
      end;

      Append (Image, "}");
      return Image;
   end Object_Image;

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
   begin
      case Data.Kind is
         when Kind_List =>
            Free_Primitive_List (Data.List_Val);
         when Kind_Iterator =>
            Data.Iter_Val.Iter.Release;
            Primitive_Iters.Free_Iterator (Data.Iter_Val.Iter);
            Free_Iterator_Primitive (Data.Iter_Val);

         when Kind_Function | Kind_Selector =>
            LKQL.Eval_Contexts.Dec_Ref
              (LKQL.Eval_Contexts.Environment_Access (Data.Frame));
         when Kind_Namespace =>
            LKQL.Eval_Contexts.Dec_Ref
              (LKQL.Eval_Contexts.Environment_Access (Data.Namespace));
         when Kind_Object =>
            Free_Primitive_Assocs (Data.Obj_Assocs);
         when Kind_Builtin_Function =>
            --  We don't ever free built-in functions, since their data is
            --  freed directly in the builtin functions package.
            null;
         when others =>
            null;
      end case;
   end Release;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter (Value : Iterator_Primitive) return Primitive_Iter_Access
   is
   begin
      return new Primitive_Iters.Iterator_Interface'Class'
           (Primitive_Iters.Iterator_Interface'Class (Value.Iter.Clone));
   end Get_Iter;

   -------------
   -- To_List --
   -------------

   function To_List (Iter : Iterator_Primitive) return Primitive
   is
      Element : Primitive;
      Inner   : Primitive_Iter_Access := Get_Iter (Iter);
      Result  : constant Primitive := Make_Empty_List;
   begin
      while Inner.Next (Element) loop
         Append (Result, Element);
      end loop;

      Inner.Release;
      Primitive_Iters.Free_Iterator (Inner);
      return Result;
   end To_List;

   -----------------
   -- To_Iterator --
   -----------------

   function To_Iterator (Value : Primitive) return Primitive_Iter'Class is
     (case Value.Kind is
      when Kind_Iterator =>
         Primitive_Iter'Class (Iter_Val (Value).Iter.Clone),
      when Kind_List =>
         Primitive_Vec_Iters.To_Iterator (Elements (Value).all),
      when Kind_Selector_List =>
         To_Iterator (To_List (Selector_List_Val (Value))),
      when others =>
         raise Assertion_Error with
           "Cannot get an iterator from a value of kind : " &
              Kind_Name (Value));

   -------------
   -- To_List --
   -------------

   function To_List (Value : Selector_List) return Primitive is
   begin
      return Result : constant Primitive := Make_Empty_List do
         for N of Value.Nodes loop
            Append (Result, To_Primitive (N));
         end loop;
      end return;
   end To_List;

   ----------
   -- Kind --
   ----------

   function Kind (Value : Primitive) return Valid_Primitive_Kind is
      (Value.Kind);

   -------------
   -- Int_Val --
   -------------

   function Int_Val (Value : Primitive) return Adaptive_Integer is
      (Value.Int_Val);

   -------------
   -- Str_Val --
   -------------

   function Str_Val (Value : Primitive) return Unbounded_Text_Type is
      (Value.Str_Val);

   --------------
   -- Bool_Val --
   --------------

   function Bool_Val (Value : Primitive) return Boolean is
      (Value.Bool_Val);

   --------------
   -- Node_Val --
   --------------

   function Node_Val (Value : Primitive) return H.AST_Node_Holder is
      (Value.Node_Val);

   --------------
   -- List_Val --
   --------------

   function List_Val (Value : Primitive) return Primitive_List_Access is
     (Value.List_Val);

   -----------------------
   -- Selector_List_Val --
   -----------------------

   function Selector_List_Val (Value : Primitive) return Selector_List is
      (Value.Selector_List_Val);

   --------------
   -- Iter_Val --
   --------------

   function Iter_Val (Value : Primitive) return Iterator_Primitive_Access is
     (Value.Iter_Val);

   --------------
   -- Elements --
   --------------

   function Elements
     (Value : Primitive) return not null Primitive_Vector_Access is
   begin
      return Value.List_Val.Elements'Access;
   end Elements;

   ------------------------
   -- Selector_List_Data --
   ------------------------

   function Selector_List_Data (Value       : Selector_List;
                                Member_Name : Text_Type) return Primitive
   is
   begin
      if Member_Name = "max_depth" then
         return To_Primitive (Value.Max_Depth);
      elsif Member_Name = "nodes" then
         return To_List (Value);
      else
         return List_Data (List_Val (To_List (Value)), Member_Name);
      end if;

   exception
      when Unsupported_Error =>
         raise Unsupported_Error with
           "No property named " & To_UTF8 (Member_Name) &
           " on values of kind " & To_String (Kind_Selector_List);
   end Selector_List_Data;

   ---------------
   -- List_Data --
   ---------------

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

   -------------------
   -- Iterator_Data --
   -------------------

   function Iterator_Data (Value : Iterator_Primitive_Access;
                           Member_Name : Text_Type) return Primitive
   is
   begin
      return List_Data (List_Val (To_List (Value.all)), Member_Name);
   end Iterator_Data;

   --------------
   -- Property --
   --------------

   function Data
     (Value : Primitive; Member_Name : Text_Type) return Primitive
   is
   begin
      case Kind (Value) is
         when Kind_Selector_List =>
            return Selector_List_Data (Selector_List_Val (Value), Member_Name);
         when Kind_List =>
            return List_Data (List_Val (Value), Member_Name);
         when Kind_Str =>
            return Str_Data (Str_Val (Value), Member_Name);
         when Kind_Iterator =>
            return Iterator_Data (Iter_Val (Value), Member_Name);
         when others =>
            raise Unsupported_Error with
              "Cannot get property on value of kind "
              & Kind_Name (Value);
      end case;
   end Data;

   ----------------
   -- Is_Nullish --
   ----------------

   function Is_Nullish (Value : Primitive) return Boolean
   is
     ((Kind (Value) = Kind_Node
      and then Value.Node_Val.Unchecked_Get.Is_Null_Node)
      or else Kind (Value) = Kind_Unit);

   ----------------
   -- Booleanize --
   ----------------

   function Booleanize (Value : Primitive) return Boolean is
   begin
      return (if (Value.Kind = Kind_Bool
              and then not Value.Bool_Val)
              or else Value.Kind = Kind_Unit
              or else (Value.Kind = Kind_Node
                and then Value.Node_Val.Unchecked_Get.Is_Null_Node)
              then False
              else True);
   end Booleanize;

   -------------------------
   -- Make_Unit_Primitive --
   -------------------------

   function Make_Unit_Primitive return Primitive is
   begin
      return Create_Primitive ((Kind => Kind_Unit, Pool => Pool));
   end Make_Unit_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Integer) return Primitive is
   begin
      return Create_Primitive
        ((Kind => Kind_Int, Int_Val => Create (Val), Pool => Pool));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------
   --
   function To_Primitive (Val : Adaptive_Integer) return Primitive is
   begin
      return Create_Primitive
        ((Kind => Kind_Int, Int_Val => Val, Pool => Pool));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Unbounded_Text_Type) return Primitive is
   begin
      return Create_Primitive
        ((Kind => Kind_Str, Str_Val => Val, Pool => Pool));
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
   begin
      return Create_Primitive
        ((Kind => Kind_Bool, Bool_Val => Val, Pool => Pool));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Node : H.AST_Node_Holder) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Node, Pool, Node));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Token : H.AST_Token_Holder) return Primitive is
   begin
      return Create_Primitive ((Kind_Token, Pool, Token));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Unit : H.AST_Unit_Holder) return Primitive is
   begin
      return Create_Primitive ((Kind_Analysis_Unit, Pool, Unit));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Primitive_Iter'Class) return Primitive is
      Val_Copy : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'(Primitive_Iter'Class (Val.Clone));

      Iter_Primitive : constant Iterator_Primitive_Access :=
        new Iterator_Primitive'(Iter => Val_Copy);
   begin
      return Create_Primitive ((Kind_Iterator, Pool, Iter_Primitive));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Selector_List) return Primitive is
   begin
      return Create_Primitive ((Kind_Selector_List, Pool, Val));
   end To_Primitive;

   -----------------------
   -- Make_Empty_Object --
   -----------------------

   function Make_Empty_Object return Primitive is
      Map : constant Primitive_Assocs_Access :=
        new Primitive_Assocs'(Elements => Primitive_Maps.Empty_Map);
   begin
      return Create_Primitive
        ((Kind => Kind_Object, Obj_Assocs => Map, Pool => Pool));
   end Make_Empty_Object;

   ---------------------
   -- Make_Empty_List --
   ---------------------

   function Make_Empty_List return Primitive is
      List : constant Primitive_List_Access :=
        new Primitive_List'(Elements => Primitive_Vectors.Empty_Vector);
   begin
      return Create_Primitive
        ((Kind => Kind_List, List_Val => List, Pool => Pool));
   end Make_Empty_List;

   ----------------------
   -- Make_Empty_Tuple --
   ----------------------

   function Make_Empty_Tuple return Primitive is
      List : constant Primitive_List_Access :=
        new Primitive_List'(Elements => Primitive_Vectors.Empty_Vector);
   begin
      return Create_Primitive
        ((Kind => Kind_Tuple, List_Val => List, Pool => Pool));
   end Make_Empty_Tuple;

   --------------------
   -- Make_Namespace --
   --------------------

   function Make_Namespace
     (N : Environment_Access; Module : L.LKQL_Node) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind      => Kind_Namespace,
          Namespace => N,
          Module    => Module,
          Pool      => Pool));
   end Make_Namespace;

   -------------------
   -- Make_Function --
   -------------------

   function Make_Function
     (Node : L.Base_Function; Env : Environment_Access) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind => Kind_Function,
          Fun_Node  => Node,
          Frame     => Env,
          Pool      => Pool));
   end Make_Function;

   -----------------------------
   -- Make_Property_Reference --
   -----------------------------

   function Make_Property_Reference
     (Node_Val     : Primitive;
      Property_Ref : H.AST_Node_Member_Ref_Holder) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind           => Kind_Property_Reference,
          Ref            => Property_Ref,
          Property_Node  => Node_Val.Node_Val,
          Pool           => Pool));
   end Make_Property_Reference;

   -------------
   -- Profile --
   -------------

   function Profile (Obj : Primitive) return Text_Type is
      Profile : Unbounded_Text_Type;
   begin
      case Obj.Kind is
         when Kind_Function =>
            Profile := To_Unbounded_Text
              (Obj.Fun_Node.P_Profile);
         when Kind_Selector =>
            Profile := To_Unbounded_Text
              ("selector " & Obj.Sel_Node.F_Name.Text);
         when Kind_Builtin_Function =>
            declare
               P : constant Builtin_Function := Obj.Builtin_Fn;
               package U renames Ada.Strings.Wide_Wide_Unbounded;
            begin
               U.Append (Profile, "@builtin fun ");
               U.Append (Profile, P.Name);
               U.Append (Profile, "(");
               for I in P.Params'Range loop
                  U.Append (Profile, P.Params (I).Name);

                  if Primitive_Options.Is_Some (P.Params (I).Default_Value)
                  then
                     U.Append
                       (Profile,
                        "=" & To_Unbounded_Text
                          (Primitive_Options.Extract
                               (P.Params (I).Default_Value)));
                  end if;

                  if I < P.Params'Last then
                     U.Append (Profile, ", ");
                  end if;
               end loop;
               U.Append (Profile, ")");
            end;
         when others =>
            null;
      end case;
      return To_Text (Profile);
   end Profile;

   ---------------------------
   -- Make_Builtin_Function --
   ---------------------------

   function Make_Builtin_Function (Fn : Builtin_Function) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Builtin_Function, Pool, Fn));
   end Make_Builtin_Function;

   -------------------
   -- Make_Selector --
   -------------------

   function Make_Selector
     (Node : L.Selector_Decl; Env : Environment_Access) return Primitive is
   begin
      return Create_Primitive
        ((Kind     => Kind_Selector,
          Sel_Node => Node,
          Frame    => Env,
          Pool     => Pool));
   end Make_Selector;

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

   procedure Extend_With_List (List      : Primitive_List_Access;
                               New_values : Primitive_List_Access);

   procedure Extend_With_Iter (List : Primitive_List_Access;
                               Iter : Iterator_Primitive_Access);

   ------------
   -- Extend --
   ------------

   procedure Extend (List, New_Value : Primitive) is
   begin
      Check_Kind (Kind_List, List);

      case Kind (New_Value) is
         when Kind_List =>
            Extend_With_List (List_Val (List), List_Val (New_Value));
         when Kind_Iterator =>
            Extend_With_Iter (List_Val (List), Iter_Val (New_Value));
         when others =>
            Append (List, New_Value);
      end case;
   end Extend;

   ----------------------
   -- Extend_With_List --
   ----------------------

   procedure Extend_With_List (List      : Primitive_List_Access;
                               New_values : Primitive_List_Access)
   is
   begin
      for E of New_values.Elements loop
         List.Elements.Append (E);
      end loop;
   end Extend_With_List;

   ----------------------
   -- Extend_With_Iter --
   ----------------------

   procedure Extend_With_Iter (List : Primitive_List_Access;
                               Iter : Iterator_Primitive_Access)
   is
      Iter_Copy       : Primitive_Iter'Class :=
        Iter.Iter.Clone;
      Current_Element : Primitive;
   begin
      while Iter_Copy.Next (Current_Element) loop
         List.Elements.Append (Current_Element);
      end loop;

      Iter_Copy.Release;
   end Extend_With_Iter;

   --------------
   -- Contains --
   --------------

   function Contains (List, Value : Primitive) return Boolean is
   begin
      Check_Kind (Kind_List, List);

      --  Since we're using smart pointers, the "=" function used by
      --  Vector.Contains checks referencial equality instead of structural
      --  equality. So the iteration "has" to be done manually.
      for Elem of List.List_Val.Elements loop
         if Deep_Equals (Elem, Value) then
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
      function Node_Image (N : H.AST_Node_Holder) return Text_Type
      is
         (if N.Unchecked_Get.Is_Null_Node
          then "null"
          else N.Unchecked_Get.Text_Image);

      package D renames Ada.Directories;
   begin
      return
        (case Kind (Val) is
            when Kind_Unit          =>
              To_Unbounded_Text (To_Text ("()")),
            when Kind_Int           =>
              Int_Image (Int_Val (Val)),
            when Kind_Str           =>
            --  TODO ??? We use Langkit_Support.Text.Image to quote the
            --  string and potentially escape chars in it, but we have
            --  to convert it back & forth from string. We should add
            --  an overload in langkit that returns a Text_Type.
           To_Unbounded_Text
             (To_Text (Image (To_Text (Str_Val (Val)),
              With_Quotes => True))),
            when Kind_Bool          =>
              Bool_Image (Bool_Val (Val)),
            when Kind_Node          =>
              To_Unbounded_Text (Node_Image (Val.Node_Val)),
            when Kind_Analysis_Unit =>
              To_Unbounded_Text
                ("<AnalysisUnit """
                 & To_Text
                    (D.Simple_Name
                       (Image (Val.Analysis_Unit_Val.Unchecked_Get.Name)))
                 & """>"),
            when Kind_Token         =>
               To_Unbounded_Text (Val.Token_Val.Unchecked_Get.Image),
            when Kind_Iterator      =>
              Iterator_Image (Iter_Val (Val).all),
            when Kind_List          =>
              List_Image (Val.List_Val.all),
            when Kind_Tuple         =>
              List_Image (Val.List_Val.all, "(", ")"),
            when Kind_Object        =>
              Object_Image (Val.Obj_Assocs.all),
            when Kind_Selector_List =>
              Selector_List_Image (Selector_List_Val (Val)),
            when Kind_Function      =>
              "function "
              & To_Unbounded_Text (To_Text (Val.Fun_Node.Image)),
            when Kind_Selector      =>
              "selector "
              & To_Unbounded_Text (To_Text (Val.Sel_Node.Image)),
            when Kind_Builtin_Function =>
              To_Unbounded_Text ("builtin function"),
            when Kind_Property_Reference =>
              To_Unbounded_Text
                ("<PropertyRef " & Node_Image (Val.Property_Node)
                 & Val.Ref.Unchecked_Get.Name & ">"),
            when Kind_Namespace        =>
              To_Unbounded_Text
               (To_Text (Env_Image
                (Eval_Contexts.Environment_Access (Val.Namespace))))
        );
   end To_Unbounded_Text;

   ---------------
   -- To_String --
   ---------------

   function To_String (Val : Valid_Primitive_Kind) return String is
   begin
      return (case Val is
                 when Kind_Unit               => "Unit",
                 when Kind_Int                => "Int",
                 when Kind_Str                => "Str",
                 when Kind_Bool               => "Bool",
                 when Kind_Node               => "Node",
                 when Kind_Token              => "Token",
                 when Kind_Analysis_Unit      => "Analysis_Unit",
                 when Kind_Iterator           => "Iterator",
                 when Kind_List               => "List",
                 when Kind_Object             => "Object",
                 when Kind_Tuple              => "Tuple",
                 when Kind_Selector_List      => "Selector List",
                 when Kind_Function           => "Function",
                 when Kind_Builtin_Function   => "Builtin Function",
                 when Kind_Selector           => "Selector",
                 when Kind_Namespace          => "Namespace",
                 when Kind_Property_Reference => "Property_Reference");
   end To_String;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Value : Primitive) return String is
   begin
      return (case Value.Kind is
                 when Kind_Node =>
                (if Value.Node_Val.Unchecked_Get.Is_Null_Node
                 then "No_Kind"
                 else
                   (Value.Node_Val.Unchecked_Get.Kind_Name)),
                 when others =>
                   To_String (Kind (Value)));
   end Kind_Name;

   -------------
   -- Display --
   -------------

   procedure Display
     (Value         : Primitive;
      New_Line      : Boolean)
   is
   begin
      case Value.Kind is
         when Kind_Str =>
            Ada.Wide_Wide_Text_IO.Put
              (To_Text (Str_Val (Value)));
            if New_Line then
               Ada.Wide_Wide_Text_IO.New_Line;
            end if;
         when others =>
            declare
               Content : constant Unbounded_Text_Type :=
                 To_Unbounded_Text (Value);
            begin
               if New_Line then
                  Put_Line (Content);
               else
                  Put (Content);
               end if;
            end;
      end case;
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

      if Int_Val (Right) = Zero then
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

      case Kind (Left) is
         when Kind_List | Kind_Tuple =>
            return Deep_Equals (List_Val (Left), List_Val (Right));
         when Kind_Node =>
            return H."=" (Left.Node_Val, Right.Node_Val);
         when others =>
            --  HACK: To discard the pool parameter and not have to rewrite the
            --  structural equality for the rest of the components, we create a
            --  fake primitive data where the pool is equal to the pool of the
            --  right item.
            declare
               Fake_Left : Primitive_Data := Left.all;
            begin
               Fake_Left.Pool := Right.Pool;
               return Fake_Left = Right.all;
            end;
      end case;
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
   begin
      case Kind (Left) is
         when Kind_Str =>
            Check_Kind (Kind_Str, Right);
            return To_Primitive (Str_Val (Left) & Str_Val (Right));
         when Kind_List =>
            Check_Kind (Kind_List, Right);
            declare
               Ret : constant Primitive := Make_Empty_List;
            begin
               for El of Left.List_Val.Elements loop
                  Ret.List_Val.Elements.Append (El);
               end loop;
               for El of Right.List_Val.Elements loop
                  Ret.List_Val.Elements.Append (El);
               end loop;
               return Ret;
            end;
         when others =>
            raise Unsupported_Error with "Wrong kind " & Kind_Name (Right);
      end case;
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

   -------------------
   -- Extract_Value --
   -------------------

   function Extract_Value
     (Obj           : Primitive;
      Key           : Text_Type;
      Ctx           : LKQL.Eval_Contexts.Eval_Context;
      Expected_Kind : Base_Primitive_Kind := No_Kind;
      Location      : L.LKQL_Node := L.No_LKQL_Node) return Primitive
   is
      Sym  : constant Symbol_Type := Ctx.Symbol (Key);
      Cur : constant Primitive_Maps.Cursor
        := Obj.Obj_Assocs.Elements.Find (Sym);
   begin
      if not Primitive_Maps.Has_Element (Cur) then
         Raise_And_Record_Error
           (Ctx,
            Make_Eval_Error
              (Location, "No key named " & Key & " in  object"));
      else
         if Expected_Kind /= No_Kind then
            LKQL.Evaluation.Check_Kind
              (Ctx, Location, Expected_Kind,
               Primitive_Maps.Element (Cur));
         end if;
         return Primitive_Maps.Element (Cur);
      end if;
   end Extract_Value;

   Has_Subpool_Been_Added : Boolean := False;

   function Create return Primitive_Pool is
      Ret : constant Primitive_Pool := new Primitive_Pool_Data;
   begin
      Pool_Stack.Append (Ret);

      if Pool_Stack.Last_Index > 1 then
         Has_Subpool_Been_Added := True;
      end if;

      return Ret;
   end Create;

   procedure Destroy (Pool : in out Primitive_Pool) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Primitive_Pool_Data, Primitive_Pool);

      procedure Free is new Ada.Unchecked_Deallocation
        (Primitive_Data, Primitive);
   begin
      if Pool = Pool_Stack.Last_Element then
         Pool_Stack.Delete_Last;
      else
         raise Constraint_Error with "Wrong call to Pool.Destroy";
      end if;

      for Prim of Pool.Primitives loop
         Release (Prim.all);
         Free (Prim);
      end loop;

      Free (Pool);
   end Destroy;

   Root_Pool : Primitive_Pool := Create;

   function Create_Primitive
     (Data : Primitive_Data) return Primitive
   is
      Ret : constant Primitive := new Primitive_Data'(Data);
   begin
      if Pool_Stack.Last_Index = 1 and then Has_Subpool_Been_Added then
         raise Constraint_Error with "Allocating into root pool";
      end if;
      Data.Pool.Primitives.Append (Ret);
      return Ret;
   end Create_Primitive;

   type Root_Pool_Control is new Ada.Finalization.Controlled with record
      Freed : Boolean := False;
   end record;

   overriding procedure Finalize (Self : in out Root_Pool_Control);

   overriding procedure Finalize (Self : in out Root_Pool_Control) is
   begin
      if not Self.Freed then
         Destroy (Root_Pool);
         Self.Freed := True;
      end if;
   end Finalize;

   Root_Pool_Control_Singleton : constant Root_Pool_Control :=
     (Ada.Finalization.Controlled with Freed => False);
   pragma Unreferenced (Root_Pool, Root_Pool_Control_Singleton);

end LKQL.Primitives;
