------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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
with Ada.Strings.Hash;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;
with LKQL.Errors;         use LKQL.Errors;

with Langkit_Support.Hashes; use Langkit_Support.Hashes;
with Langkit_Support.Names; use Langkit_Support.Names;

with GNAT.Case_Util;

with LKQL.Depth_Nodes;

package body LKQL.Primitives is

   function Int_Image (Value : Adaptive_Integer) return Unbounded_Text_Type;
   --  Wraps the Integer'Wide_Wide_Image function, removing the leading space

   function Bool_Image (Value : Boolean) return Unbounded_Text_Type;
   --  Return a String representation of the given Boolean value

   function Iterator_Image
     (Value : Primitive) return Unbounded_Text_Type;

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

   function Selector_List_Data (Value       : Selector_List;
                                Member_Name : Text_Type;
                                Pool        : Primitive_Pool) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Primitive Selector_List.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

   function List_Data (Value       : Primitive_List_Access;
                       Member_Name : Text_Type;
                       Pool        : Primitive_Pool) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Primitive List.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

   function Str_Data
     (Value       : Text_Type;
      Member_Name : Text_Type;
      Pool        : Primitive_Pool) return Primitive;
   --  Return the value of the property named 'Member_Name' of the given
   --  Str value.
   --  Raise an Unsupported_Error if there is no property named
   --  'Member_Name'.

   procedure Raise_Unsupported_Operation
     (Left, Right : Primitive; Name : String)
     with No_Return;
   --  Raise an Unsupported_Operation exception mentionning the kind of the
   --  operands as well as the name of the operation.

   function Create return Primitive_Pool;
   --  Create a new primitive pool

   procedure Destroy (Pool : in out Primitive_Pool);
   --  Destroy the pool (free all the objects, and free the pool)

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
      use LKQL.Depth_Nodes;
      Image   : Unbounded_Text_Type;
      Nodes   : constant Depth_Node_Vector := Value.Depth_Nodes;
      Length  : constant Count_Type := Nodes.Length;
      Counter : Count_Type := 1;
   begin
      Append (Image, "[");

      for D of Nodes loop
         Counter := Counter + 1;
         Append (Image, To_Text (D.Node.Image));
         if Counter <= Length then
            Append (Image, ", ");
         end if;
      end loop;

      Append (Image, "]");

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
     (Value : Primitive) return Unbounded_Text_Type
   is
   begin
      Consume (Value);
      return List_Image (Value.Iter_Cache.all);
   end Iterator_Image;

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Value : Primitive) is
   begin
      if Kind (Value) /= Expected_Kind then
         raise Unsupported_Error
           with "Type error: expected " &
            To_String (Expected_Kind) & " but got " &
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
      procedure Free_Regex is new Ada.Unchecked_Deallocation
        (GNAT.Regpat.Pattern_Matcher, Regex_Access);
   begin
      case Data.Kind is
         when Kind_List =>
            Free_Primitive_List (Data.List_Val);
         when Kind_Iterator =>
            Data.Iter_Val.Iter.Release;
            Primitive_Iters.Free_Iterator (Data.Iter_Val.Iter);
            Free_Iterator_Primitive (Data.Iter_Val);
            Free_Primitive_List (Data.Iter_Cache);
         when Kind_Function | Kind_Selector =>
            LKQL.Eval_Contexts.Dec_Ref
              (LKQL.Eval_Contexts.Environment_Access (Data.Frame));
         when Kind_Str =>
            Free (Data.Str_Val);
         when Kind_Namespace =>
            LKQL.Eval_Contexts.Dec_Ref
              (LKQL.Eval_Contexts.Environment_Access (Data.Namespace));
         when Kind_Object =>
            Free_Primitive_Assocs (Data.Obj_Assocs);
         when Kind_Builtin_Function =>
            --  We don't ever free built-in functions, since their data is
            --  freed directly in the builtin functions package.
            null;
         when Kind_Regex =>
            Free_Regex (Data.Regex_Val);
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
   -- Consume --
   -------------

   procedure Consume (Iter : Primitive; Num_Elements : Integer := -1)
   is
      Element  : Primitive;
      Consumed : Natural := 0;
   begin
      while (Num_Elements = -1 or else Consumed < Num_Elements)
         and then Iter.Iter_Val.Iter.Next (Element)
      loop
         Iter.Iter_Cache.Elements.Append (Element);
         Consumed := Consumed + 1;
      end loop;
   end Consume;

   -----------------
   -- To_Iterator --
   -----------------

   function To_Iterator
     (Value : Primitive; Pool : Primitive_Pool) return Primitive_Iter'Class
   is
     (case Value.Kind is
      when Kind_Iterator =>
         Primitive_Iter'Class (Iter_Val (Value).Iter.Clone),
      when Kind_List =>
         Primitive_Vec_Iters.To_Iterator (Elements (Value).all),
      when Kind_Selector_List =>
         To_Iterator (To_List (Selector_List_Val (Value), Pool), Pool),
      when others =>
         raise Assertion_Error with
           "Cannot get an iterator from a value of kind : " &
              Kind_Name (Value));

   -------------
   -- To_List --
   -------------

   function To_List
     (Value : Selector_List; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Result : constant Primitive := Make_Empty_List (Pool) do
         for N of Value.Nodes loop
            Append (Result, To_Primitive (N, Pool));
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

   function Str_Val (Value : Primitive) return Text_Type is
      (Value.Str_Val.all);

   --------------
   -- Bool_Val --
   --------------

   function Bool_Val (Value : Primitive) return Boolean is
      (Value.Bool_Val);

   --------------
   -- Node_Val --
   --------------

   function Node_Val (Value : Primitive) return LK.Lk_Node is
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
      case Value.Kind is
         when Kind_List =>
            return Value.List_Val.Elements'Access;
         when Kind_Iterator =>
            Consume (Value);
            return Value.Iter_Cache.Elements'Access;
         when others =>
            raise Unsupported_Error with "Invalid kind for elements";
      end case;
   end Elements;

   ------------------------
   -- Selector_List_Data --
   ------------------------

   function Selector_List_Data
     (Value       : Selector_List;
      Member_Name : Text_Type;
      Pool        : Primitive_Pool) return Primitive
   is
   begin
      if Member_Name = "max_depth" then
         return To_Primitive (Value.Max_Depth, Pool);
      elsif Member_Name = "nodes" then
         return To_List (Value, Pool);
      else
         return List_Data
           (List_Val (To_List (Value, Pool)), Member_Name, Pool);
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

   function List_Data (Value       : Primitive_List_Access;
                       Member_Name : Text_Type;
                       Pool        : Primitive_Pool) return Primitive
   is
   begin
      if Member_Name = "length" then
         return To_Primitive (Integer (Value.Elements.Length), Pool);
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
     (Value       : Text_Type;
      Member_Name : Text_Type;
      Pool        : Primitive_Pool) return Primitive
   is
   begin
      if Member_Name = "length" then
         return To_Primitive (Value'Length, Pool);
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
     (Value       : Primitive;
      Member_Name : Text_Type;
      Pool        : Primitive_Pool) return Primitive
   is
   begin
      case Kind (Value) is
         when Kind_Selector_List =>
            return Selector_List_Data
              (Selector_List_Val (Value), Member_Name, Pool);
         when Kind_List =>
            return List_Data (List_Val (Value), Member_Name, Pool);
         when Kind_Str =>
            return Str_Data (Str_Val (Value), Member_Name, Pool);
         when Kind_Iterator =>
            Consume (Value);
            return List_Data (Value.Iter_Cache, Member_Name, Pool);
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
      and then Value.Node_Val.Is_Null)
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
                and then Value.Node_Val.Is_Null)
              then False
              else True);
   end Booleanize;

   ------------
   -- Truthy --
   ------------

   function Truthy (Value : Primitive; Has_Truthy : out Boolean) return Boolean
   is
   begin
      Has_Truthy := True;

      case Kind (Value) is
         when Kind_Node | Kind_Unit =>
            return Booleanize (Value);
         when Kind_Bool =>
            return Bool_Val (Value);
         when Kind_Iterator =>
            declare
               Iterator_Clone : Primitive_Iters.Iterator_Interface'Class
                 := Primitive_Iters.Clone (Iter_Val (Value).Iter.all);
               Dummy_Element : Primitive;
            begin
               return Primitive_Iters.Next (Iterator_Clone, Dummy_Element);
            end;
         when Kind_List =>
            return Primitives.Length (Value) /= 0;
         when others =>
            Has_Truthy := False;
            return False;
      end case;
   end Truthy;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Val : Integer; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind => Kind_Int, Int_Val => Create (Val), Pool => Pool));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------
   --
   function To_Primitive
     (Val : Adaptive_Integer; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind => Kind_Int, Int_Val => Val, Pool => Pool));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Val : Text_Type; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind => Kind_Str, Str_Val => new Text_Type'(Val), Pool => Pool));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Node : LK.Lk_Node; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Node, Pool, Node));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Token : LK.Lk_Token; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Token, Pool, Token));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Unit : LK.Lk_Unit; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Analysis_Unit, Pool, Unit));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Val : Primitive_Iter'Class; Pool : Primitive_Pool) return Primitive
   is
      Val_Copy : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'(Primitive_Iter'Class (Val.Clone));

      Iter_Primitive : constant Iterator_Primitive_Access :=
        new Iterator_Primitive'(Iter => Val_Copy);

      List : constant Primitive_List_Access :=
        new Primitive_List'(Elements => Primitive_Vectors.Empty_Vector);
   begin
      return Create_Primitive
        ((Kind_Iterator, Pool, Iter_Primitive, Iter_Cache => List));
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Val : Selector_List; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Selector_List, Pool, Val));
   end To_Primitive;

   -----------------------
   -- Make_Empty_Object --
   -----------------------

   function Make_Empty_Object (Pool : Primitive_Pool) return Primitive is
      Map : constant Primitive_Assocs_Access :=
        new Primitive_Assocs'(Elements => Primitive_Maps.Empty_Map);
   begin
      return Create_Primitive
        ((Kind => Kind_Object, Obj_Assocs => Map, Pool => Pool));
   end Make_Empty_Object;

   ---------------------
   -- Make_Empty_List --
   ---------------------

   function Make_Empty_List (Pool : Primitive_Pool) return Primitive is
      List : constant Primitive_List_Access :=
        new Primitive_List'(Elements => Primitive_Vectors.Empty_Vector);
   begin
      return Create_Primitive
        ((Kind => Kind_List, List_Val => List, Pool => Pool));
   end Make_Empty_List;

   ----------------------
   -- Make_Empty_Tuple --
   ----------------------

   function Make_Empty_Tuple (Pool : Primitive_Pool) return Primitive is
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
     (N      : Environment_Access;
      Module : L.Lkql_Node;
      Pool   : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind      => Kind_Namespace,
          Namespace => N,
          Module    => Module,
          Pool      => Pool));
   end Make_Namespace;

   ----------------
   -- Make_Regex --
   ----------------

   function Make_Regex
     (Regex : GNAT.Regpat.Pattern_Matcher;
      Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind      => Kind_Regex,
          Regex_Val => new GNAT.Regpat.Pattern_Matcher'(Regex),
          Pool      => Pool));
   end Make_Regex;

   -------------------
   -- Make_Function --
   -------------------

   function Make_Function
     (Node            : L.Base_Function;
      Env             : Environment_Access;
      Pool            : Primitive_Pool;
      With_Call_Cache : Boolean := False) return Primitive is
   begin
      return Create_Primitive
        ((Kind       => Kind_Function,
          Fun_Node   => Node,
          Frame      => Env,
          Pool       => Pool,
          Call_Cache => (if With_Call_Cache
                        then Callable_Caches.Create (Pool)
                        else Callable_Caches.No_Cache)));
   end Make_Function;

   -----------------------------
   -- Make_Property_Reference --
   -----------------------------

   function Make_Property_Reference
     (Node_Val     : LK.Lk_Node;
      Property_Ref : LKI.Struct_Member_Ref;
      Pool         : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind           => Kind_Property_Reference,
          Ref            => Property_Ref,
          Property_Node  => Node_Val,
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

   function Make_Builtin_Function
     (Fn : Builtin_Function; Pool : Primitive_Pool) return Primitive
   is
   begin
      return Create_Primitive ((Kind_Builtin_Function, Pool, Fn));
   end Make_Builtin_Function;

   -------------------
   -- Make_Selector --
   -------------------

   function Make_Selector
     (Node            : L.Selector_Decl;
      Env             : Environment_Access;
      Pool            : Primitive_Pool;
      With_Call_Cache : Boolean := False) return Primitive
   is
   begin
      return Create_Primitive
        ((Kind      => Kind_Selector,
          Sel_Node  => Node,
          Frame     => Env,
          Pool      => Pool,
          Sel_Cache => (if With_Call_Cache
                         then new Node_To_Nodes.Map
                         else null)));
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

   function Contains
     (List, Value : Primitive) return Boolean
   is
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

   function Get
     (List : Primitive; Index : Integer;
      Raise_If_OOB : Boolean := True) return Primitive
   is
   begin
      return Get (List.List_Val, Index, Raise_If_OOB);
   end Get;

   function Get
     (List : Primitive_List_Access; Index : Integer;
      Raise_If_OOB : Boolean := True) return Primitive
   is
   begin
      if Index not in List.Elements.First_Index .. List.Elements.Last_Index
      then
         if Raise_If_OOB then
            raise Unsupported_Error
              with "Invalid index: " & Integer'Image (Index);
         else
            return Make_Unit_Primitive;
         end if;

      end if;

      return List.Elements.Element (Positive (Index));
   end Get;

   ------------
   -- Length --
   ------------

   function Length (List : Primitive) return Natural is
   begin
      Check_Kind (Kind_List, List);
      return Natural (Elements (List).Length);
   end Length;

   ---------------
   -- To_String --
   ---------------

   function To_String (Val : Primitive) return String is
   begin
      return Image (To_Text (To_Unbounded_Text (Val)));
   end To_String;

   -----------------------
   -- To_Unbounded_Text --
   -----------------------

   function To_Unbounded_Text (Val : Primitive) return Unbounded_Text_Type is
      function Node_Image (N : LK.Lk_Node) return Text_Type
      is
         (if N.Is_Null
          then "null"
          else To_Text (N.Image));

      package D renames Ada.Directories;

      Pool : Primitive_Pool := Create;
   begin
      return T : Unbounded_Text_Type do
         T := (case Kind (Val) is
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
                (To_Text (Image (Str_Val (Val),
                 With_Quotes => True))),
               when Kind_Regex => To_Unbounded_Text ("<regex>"),
               when Kind_Bool          =>
                 Bool_Image (Bool_Val (Val)),
               when Kind_Node          =>
                 To_Unbounded_Text (Node_Image (Val.Node_Val)),
               when Kind_Analysis_Unit =>
                 To_Unbounded_Text
                   ("<AnalysisUnit """
                    & To_Text
                       (D.Simple_Name
                          (Val.Analysis_Unit_Val.Filename))
                    & """>"),
               when Kind_Token         =>
                  To_Unbounded_Text (To_Text (Val.Token_Val.Image)),
               when Kind_Iterator      =>
                 Iterator_Image (Val),
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
                    & Format_Name (LKI.Member_Name (Val.Ref), Lower) & ">"),
                    --  TODO: Use Struct_Member_Ref.Name when it is implemented
               when Kind_Namespace        =>
                 To_Unbounded_Text
                  (To_Text (Env_Image
                   (Eval_Contexts.Environment_Access (Val.Namespace)))));
         Destroy (Pool);
      end return;
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
                 when Kind_Regex              => "Regex",
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
                (if Value.Node_Val.Is_Null
                 then "No_Kind"
                 else
                   (LKI.Debug_Name (LKI.Type_Of (Value.Node_Val)))),
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
              (Str_Val (Value));
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

   ------------
   -- Equals --
   ------------

   function Equals (Left, Right : Primitive) return Primitive is
      (To_Primitive (Deep_Equals (Left, Right)));

   function Equals (Left, Right : Primitive) return Boolean is
   begin
      if Left = null then
         return Right = null;
      end if;

      return Left.Kind = Right.Kind and then Deep_Equals (Left, Right);
   end Equals;

   -----------------
   -- Deep_Equals --
   -----------------

   function Deep_Equals
     (Left, Right : Primitive) return Boolean
   is
   begin
      if Kind (Left) /= Kind (Right) then
         return False;
      end if;

      case Kind (Left) is
         when Kind_List | Kind_Tuple =>
            return Deep_Equals (List_Val (Left), List_Val (Right));
         when Kind_Node =>
            return LK."=" (Left.Node_Val, Right.Node_Val);
         when Kind_Str =>
            return Left.Str_Val.all = Right.Str_Val.all;
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

   function Deep_Equals
     (Left, Right : Primitive_List_Access) return Boolean
   is
   begin
      if Left.Elements.Length /= Right.Elements.Length then
         return False;
      end if;

      for I in Left.Elements.First_Index .. Left.Elements.Last_Index loop
         if not Bool_Val (Equals (Left.Elements (I), Right.Elements (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Deep_Equals;

   ---------
   -- "&" --
   ---------

   function Concat
     (Left, Right : Primitive;
      Pool : Primitive_Pool) return Primitive
   is
   begin
      case Kind (Left) is
         when Kind_Str =>
            Check_Kind (Kind_Str, Right);
            return To_Primitive (Str_Val (Left) & Str_Val (Right), Pool);
         when Kind_List =>
            Check_Kind (Kind_List, Right);
            declare
               Ret : constant Primitive := Make_Empty_List (Pool);
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
   end Concat;

   ---------
   -- "<" --
   ---------

   function Lt
     (Left, Right : Primitive) return Primitive
   is
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
   end Lt;

   ----------
   -- "<=" --
   ----------

   function Lte
     (Left, Right : Primitive) return Primitive
   is
      Is_Lt : constant Primitive := Lt (Left, Right);
   begin
      if Bool_Val (Is_Lt) then
         return Is_Lt;
      else
         return Equals (Left, Right);
      end if;
   end Lte;

   ---------
   -- ">" --
   ---------

   function Gt
     (Left, Right : Primitive) return Primitive
   is
     (To_Primitive
        (not (Bool_Val (Lt (Left, Right))
         or else Bool_Val (Equals (Left, Right)))));

   ----------
   -- ">=" --
   ----------

   function Gte
     (Left, Right : Primitive) return Primitive
   is
      (To_Primitive (not Bool_Val (Lt (Left, Right))));

   -------------------
   -- Extract_Value --
   -------------------

   function Extract_Value
     (Obj           : Primitive;
      Key           : Text_Type;
      Ctx           : LKQL.Eval_Contexts.Eval_Context;
      Expected_Kind : Base_Primitive_Kind := No_Kind;
      Location      : L.Lkql_Node := L.No_Lkql_Node) return Primitive
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

   function Create return Primitive_Pool_Stack is
      Ret : Primitive_Pool_Stack;
   begin
      Ret := new Primitive_Pool_Vectors.Vector;
      Ret.Append (Create);
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Primitive_Pool is
      Ret : constant Primitive_Pool := new Primitive_Pool_Data;
   begin
      return Ret;
   end Create;

   ----------
   -- Mark --
   ----------

   procedure Mark (Pool_Stack : Primitive_Pool_Stack) is
   begin
      Pool_Stack.Append (Create);
   end Mark;

   -------------
   -- Release --
   -------------

   procedure Release (Pool_Stack : Primitive_Pool_Stack) is
      Last_Pool : Primitive_Pool := Pool_Stack.Last_Element;
   begin
      Destroy (Last_Pool);
      Pool_Stack.Delete_Last;
   end Release;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Pool : in out Primitive_Pool) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Primitive_Pool_Data, Primitive_Pool);
      procedure Free is new Ada.Unchecked_Deallocation
        (Primitive_Data, Primitive);
   begin
      for Prim of Pool.Primitives loop
         Release (Prim.all);
         Free (Prim);
      end loop;
      Free (Pool);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Primitive_Pool_Stack) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Primitive_Pool_Vectors.Vector, Primitive_Pool_Stack);
   begin
      while Self.Length > 0 loop
         Release (Self);
      end loop;

      Free (Self);
   end Destroy;

   Root_Pool : Primitive_Pool := Create;

   ----------------------
   -- Create_Primitive --
   ----------------------

   function Create_Primitive
     (Data : Primitive_Data) return Primitive
   is
      Ret : constant Primitive := new Primitive_Data'(Data);
   begin
      Data.Pool.Primitives.Append (Ret);
      return Ret;
   end Create_Primitive;

   type Root_Pool_Control is new Ada.Finalization.Controlled with record
      Freed : Boolean := False;
   end record;

   overriding procedure Finalize (Self : in out Root_Pool_Control);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Root_Pool_Control) is
   begin
      if not Self.Freed then
         Destroy (Root_Pool);
         Self.Freed := True;
      end if;
   end Finalize;

   Root_Pool_Control_Singleton : constant Root_Pool_Control :=
     (Ada.Finalization.Controlled with Freed => False);
   pragma Unreferenced (Root_Pool_Control_Singleton);

   False_Prim : constant Primitive :=
      Create_Primitive
        ((Kind => Kind_Bool, Bool_Val => False, Pool => Root_Pool));

   True_Prim : constant Primitive :=
      Create_Primitive
        ((Kind => Kind_Bool, Bool_Val => True, Pool => Root_Pool));

   Unit_Prim : constant Primitive :=
      Create_Primitive ((Kind => Kind_Unit, Pool => Root_Pool));

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Boolean) return Primitive is
   begin
      return (case Val is
              when True => True_Prim,
              when False => False_Prim);
   end To_Primitive;

   -------------------------
   -- Make_Unit_Primitive --
   -------------------------

   function Make_Unit_Primitive return Primitive
   is
   begin
      return Unit_Prim;
   end Make_Unit_Primitive;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Primitive) return Hash_Type is
   begin
      case Self.Kind is
         when Kind_Unit =>
            return Hash_Type (0);
         when Kind_Int =>
            return Ada.Strings.Hash (Image (Self.Int_Val));
         when Kind_Str =>
            return Hash (Self.Str_Val.all);
         when Kind_Regex =>
            raise Constraint_Error with "Hash not supported on regex";
         when Kind_Bool =>
            return Hash_Type (Boolean'Pos (Self.Bool_Val));
         when Kind_Node =>
            return LK.Hash (Self.Node_Val);
         when Kind_Analysis_Unit =>
            return LK.Hash (Self.Analysis_Unit_Val);
         when Kind_Iterator =>
            raise Constraint_Error with "Hash not supported on iterators";
         when Kind_Token =>
            raise Constraint_Error with "Hash not yet supported on tokens";
         when Kind_List | Kind_Tuple =>
            declare
               L : Primitive_Vectors.Vector renames Self.List_Val.Elements;
               Hashes : Hash_Array (L.First_Index .. L.Last_Index);
            begin
               for I in L.First_Index .. L.Last_Index loop
                  Hashes (I) := Hash (L (I));
               end loop;
               return Combine (Hashes);
            end;
         when Kind_Object =>
            declare
               L : Primitive_Maps.Map renames Self.Obj_Assocs.Elements;
               Hashes : Hash_Array (1 .. Integer (L.Length));
               I : Integer := 1;
            begin
               for It in L.Iterate loop
                  Hashes (I) := Hash (Primitive_Maps.Element (It));
                  I := I + 1;
               end loop;
               return Combine (Hashes);
            end;
         when Kind_Selector_List =>
            raise Constraint_Error with "Selector list not hashable";
         when Kind_Builtin_Function =>
            raise Constraint_Error with "Builtin function not hashable";
         when Kind_Property_Reference =>
            raise Constraint_Error with "Property reference not hashable";
         when Kind_Namespace =>
            raise Constraint_Error with "Namespace not hashable";
         when Kind_Function | Kind_Selector =>
            raise Constraint_Error with "Callables not hashable";
      end case;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Vec : Primitive_Vectors.Vector) return Hash_Type is
      Hashes : Hash_Array (Vec.First_Index .. Vec.Last_Index);
   begin
      for I in Vec.First_Index .. Vec.Last_Index loop
         Hashes (I) := Hash (Vec (I));
      end loop;
      return Combine (Hashes);
   end Hash;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Primitive; Pool : Primitive_Pool) return Primitive is
   begin
      case Self.Kind is
         when Kind_Unit | Kind_Bool =>
            --  Unit, True & False are singleton allocated in a global pool.
            --  Don't copy them.
            return Self;
         when Kind_Int =>
            return Create_Primitive
              ((Kind => Kind_Int, Int_Val => Self.Int_Val, Pool => Pool));
         when Kind_Str =>
            return Create_Primitive
              ((Kind => Kind_Str,
                Str_Val => new Text_Type'(Self.Str_Val.all), Pool => Pool));
         when Kind_Node =>
            return Create_Primitive
              ((Kind => Kind_Node, Node_Val => Self.Node_Val, Pool => Pool));
         when Kind_Analysis_Unit =>
            return Create_Primitive
              ((Kind => Kind_Analysis_Unit,
                Analysis_Unit_Val => Self.Analysis_Unit_Val, Pool => Pool));
         when Kind_Iterator =>
            raise Constraint_Error with "Copy not supported on iterators";
         when Kind_Regex =>
            raise Constraint_Error with "Copy not supported on regex";
         when Kind_Token =>
            return Create_Primitive
              ((Kind => Kind_Token, Token_Val => Self.Token_Val,
                Pool => Pool));
         when Kind_List | Kind_Tuple =>
            declare
               L : Primitive_Vectors.Vector renames Self.List_Val.Elements;
               New_List : Primitive_Vectors.Vector;
               New_Primitive_Val : Primitive_Data := Self.all;
            begin
               New_List.Set_Length (Count_Type (L.Last_Index));
               for I in L.First_Index .. L.Last_Index loop
                  New_List (I) := Copy (L (I), Pool);
               end loop;
               New_Primitive_Val.Pool := Pool;
               New_Primitive_Val.List_Val :=
                 new Primitive_List'(Elements => New_List);
               return Create_Primitive (New_Primitive_Val);
            end;
         when Kind_Object =>
            declare
               L : Primitive_Maps.Map renames Self.Obj_Assocs.Elements;
               New_Map : Primitive_Maps.Map;
            begin
               for It in L.Iterate loop
                  New_Map.Include
                    (Primitive_Maps.Key (It),
                     Copy (Primitive_Maps.Element (It), Pool));
               end loop;
               return Create_Primitive
                 ((Kind => Kind_Object,
                   Obj_Assocs => new Primitive_Assocs'(Elements => New_Map),
                   Pool => Pool));
            end;
         when Kind_Selector_List =>
            raise Constraint_Error with "Selector list not copyable";
         when Kind_Builtin_Function =>
            raise Constraint_Error with "Builtin function not copyable";
         when Kind_Property_Reference =>
            raise Constraint_Error with "Property reference not copyable";
         when Kind_Namespace =>
            raise Constraint_Error with "Namespace not copyable";
         when Kind_Function | Kind_Selector =>
            raise Constraint_Error with "Callables not copyable";
      end case;
   end Copy;

   package body Callable_Caches is

      ------------
      -- Create --
      ------------

      function Create (Pool : Primitive_Pool) return Cache is
      begin
         return new Cache_Data'(Pool => Pool, Cache => <>);
      end Create;

      -----------
      -- Query --
      -----------

      function Query
        (Self : Cache; Args : Primitive_Vectors.Vector) return Primitive
      is
         C : constant Cache_Maps.Cursor := Self.Cache.Find (Args);
      begin
         if Cache_Maps.Has_Element (C) then
            return Cache_Maps.Element (C);
         end if;
         return null;
      end Query;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Self : Cache; Args : Primitive_Vectors.Vector; Value : Primitive)
      is
         Copied_Args : Primitive_Vectors.Vector;
      begin
         for A of Args loop
            Copied_Args.Append (Copy (A, Self.Pool));
         end loop;
         Self.Cache.Include (Copied_Args, Copy (Value, Self.Pool));
      end Insert;

   end Callable_Caches;

end LKQL.Primitives;
