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

with LKQL.Primitives; use LKQL.Primitives;

package body LKQL.AST_Nodes is

   ----------------------
   -- Make_AST_Node_Rc --
   ----------------------

   function Make_AST_Node_Rc (Node : AST_Node'Class) return AST_Node_Rc is
      Ref : AST_Node_Ptrs.Ref;
   begin
      Ref.Set (Node);
      return Ref;
   end Make_AST_Node_Rc;

   ----------------------
   -- Make_Ast_Node_Rc --
   ----------------------

   function Make_AST_Node_Rc
     (Node : AST_Node_Access) return AST_Node_Rc
   is
      Copy_Rc : constant AST_Node_Rc := Make_AST_Node_Rc (Node.all);
   begin
      return Copy_Rc;
   end Make_AST_Node_Rc;

   ----------------------------
   -- Release_AST_Node_Array --
   ----------------------------

   procedure Release_AST_Node_Array (Value : in out AST_Node_Array_Access) is
   begin
      for N of Value.all loop
         Free_AST_Node (N);
      end loop;

      Free_Ast_Node_Array (Value);
   end Release_AST_Node_Array;

   ---------------------------------
   -- Release_Introspection_Value --
   ---------------------------------

   procedure Release_Introspection_Value (Value : in out Introspection_Value)
   is
   begin
      case Value.Kind is
         when Kind_Node_Array =>
            Release_AST_Node_Array (Value.Node_Array_Val);
         when Kind_Text_Array =>
            Free_Unbounded_Text_Array (Value.Text_Array_Val);
         when others => null;
      end case;
   end Release_Introspection_Value;

   procedure Add_Children (Iter : in out Child_Iterator; Node : AST_Node_Rc);
   procedure Initialize_Next_Elements (Iter : in out Child_Iterator);

   ----------------------------
   -- To_Introspection_Value --
   ----------------------------

   function To_Introspection_Value (Val : Boolean) return Introspection_Value
   is
   begin
      return (Kind => Kind_Bool, Bool_Val => Val);
   end To_Introspection_Value;

   ----------------------------
   -- To_Introspection_Value --
   ----------------------------

   function To_Introspection_Value
     (Val : Adaptive_Integer) return Introspection_Value
   is
   begin
      return (Kind => Kind_Int, Int_Val => +Val);
   end To_Introspection_Value;

   ----------------------------
   -- To_Introspection_Value --
   ----------------------------

   function To_Introspection_Value
     (Val : Unbounded_Text_Type) return Introspection_Value
   is
   begin
      return (Kind => Kind_Text, Text_Val => Val);
   end To_Introspection_Value;

   ----------------------------
   -- To_Introspection_Value --
   ----------------------------

   function To_Introspection_Value
     (Val : AST_Node_Rc) return Introspection_Value
   is
   begin
      return (Kind     => Kind_Node,
              Node_Val => new AST_Node'Class'(Val.Unchecked_Get.all));
   end To_Introspection_Value;

   ----------------------------
   -- To_Introspection_Value --
   ----------------------------

   function To_Introspection_Value
     (Val : LKQL.Primitives.Primitive_List_Access) return Introspection_Value
   is
      Elements : constant Primitive_Vectors.Vector := Val.Elements;
   begin
      if Elements.Is_Empty then
         return Introspection_Value'(Kind => Kind_Empty_List);
      end if;

      case Kind (Elements.First_Element) is
         when Kind_Node =>
            declare
               Result : constant AST_Node_Array_Access :=
                 new AST_Node_Array (1 .. Integer (Elements.Length));
            begin
               for I in 1 .. Integer (Elements.Length) loop
                  Result (I) := new AST_Node'Class'
                    (Node_Val (Elements.Element (I)).Unchecked_Get.all);
               end loop;

               return (Kind => Kind_Node_Array, Node_Array_Val => Result);
            end;

         when Kind_Str =>
            declare
               Result : constant Unbounded_Text_Array_Access :=
                 new Unbounded_Text_Array (1 .. Integer (Elements.Length));
            begin
               for I in 1 .. Integer (Elements.Length) loop
                  Result (I) := Str_Val (Elements.Element (I));
               end loop;

               return (Kind => Kind_Text_Array, Text_Array_Val => Result);
            end;

         when others =>
            raise Unsupported_Error with "Cannot create a Value array from" &
              " a list of " & To_String (Kind (Elements.First_Element)) &
              " values";
      end case;
   end To_Introspection_Value;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : in out Child_Iterator; Result : out AST_Node_Rc) return Boolean
   is
   begin
      if Iter.Next_Elements.Is_Empty then
         return False;
      end if;

      --  This implements a DFS traversal, so that given the tree:
      --
      --  a __ b _ d
      --  \    \__ e
      --   \__ c
      --
      --  This will return the list [a, b, d, e, c]
      --
      --  This works by adding children on the "stack" in reverse order, and
      --  picking the last one everytime:
      --
      --  [a]       elem=a (Initial state)
      --  [c, b]    elem=b
      --  [c, e, d] elem=d
      --  [c, e]    elem=e
      --  [c]       elem=c

      Result := Iter.Next_Elements.Last_Element;
      Iter.Next_Elements.Delete_Last;
      Add_Children (Iter, Result);

      return True;
   end Next;

   ------------------
   -- Add_Children --
   ------------------

   procedure Add_Children (Iter : in out Child_Iterator; Node : AST_Node_Rc) is
   begin
      for I in reverse 1 .. Node.Get.Children_Count loop
         if not Node.Get.Nth_Child (I).Is_Null_Node then
            Iter.Next_Elements.Append
              (Make_AST_Node_Rc (Node.Get.Nth_Child (I)));
         end if;
      end loop;
   end Add_Children;

   ------------------------------
   -- Initialize_Next_Elements --
   ------------------------------

   procedure Initialize_Next_Elements (Iter : in out Child_Iterator) is
   begin
      for El of Iter.Roots loop
         if not El.Get.Is_Null_Node then
            Iter.Next_Elements.Append (El);
         end if;
      end loop;
   end Initialize_Next_Elements;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Child_Iterator) return Child_Iterator is
      Res : Child_Iterator := (Roots => Iter.Roots, others => <>);
   begin
      Initialize_Next_Elements (Res);
      return Res;
   end Clone;

   -------------------------
   -- Make_Child_Iterator --
   -------------------------

   function Make_Child_Iterator
     (Nodes : AST_Node_Array) return Child_Iterator
   is
      Result       : Child_Iterator;
   begin
      for Node of Nodes loop
         declare
            Rc : constant AST_Node_Rc := Make_AST_Node_Rc (Node);
         begin
            Result.Roots.Append (Rc);
         end;
      end loop;
      Initialize_Next_Elements (Result);
      return Result;
   end Make_Child_Iterator;

   -------------------------
   -- Make_Child_Iterator --
   -------------------------

   function Make_Child_Iterator
     (Nodes : AST_Node_Vector) return Child_Iterator
   is
      Result       : Child_Iterator;
   begin
      Result.Roots := Nodes;
      Initialize_Next_Elements (Result);
      return Result;
   end Make_Child_Iterator;

end LKQL.AST_Nodes;
