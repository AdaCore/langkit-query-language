------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                        Copyright (C) 2021-2022, AdaCore                  --
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

with LKQL.AST_Nodes; use LKQL.AST_Nodes;

package body LKQL.Partial_AST_Nodes is

   package body H is
      package Node_Impl is new Unbounded_Holders.Holders_Impl
        (LKQL.AST_Nodes.AST_Node'Class,
         AST_Node_Access,
         AST_Node_Holders);

      -------------------
      -- Unchecked_Get --
      -------------------

      function Unchecked_Get (Self : AST_Node_Holder) return AST_Node_Access is
      begin
         return Node_Impl.Unchecked_Get (AST_Node_Holders.Holder (Self));
      end Unchecked_Get;

      ------------
      -- Create --
      ------------

      function Create_Node (Value : AST_Node'Class) return AST_Node_Holder is
      begin
         return (Node_Impl.Create (Value) with null record);
      end Create_Node;

      ---------
      -- "=" --
      ---------

      function "=" (L, R : AST_Node_Holder) return Boolean is
      begin
         return L.Unchecked_Get.all = R.Unchecked_Get.all;
      end "=";

      ----------
      -- Hash --
      ----------

      function Hash (Self : AST_Node_Holder) return Hash_Type is
      begin
         return LKQL.AST_Nodes.Hash (Self.Unchecked_Get.all);
      end Hash;

      ----------
      -- Hash --
      ----------

      function Hash (Self : AST_Unit_Holder) return Hash_Type is
      begin
         return LKQL.AST_Nodes.Hash (Self.Unchecked_Get.all);
      end Hash;

      package Ref_Impl is new Unbounded_Holders.Holders_Impl
        (AST_Node_Member_Reference'Class,
         AST_Node_Member_Ref_Access,
         AST_Node_Member_Ref_Holders);

      -------------------
      -- Unchecked_Get --
      --------------------

      function Unchecked_Get
        (Self : AST_Node_Member_Ref_Holder) return AST_Node_Member_Ref_Access
      is
      begin
         return Ref_Impl.Unchecked_Get
           (AST_Node_Member_Ref_Holders.Holder (Self));
      end Unchecked_Get;

      ------------
      -- Create --
      ------------

      function Create_Member_Ref
        (Value : LKQL.AST_Nodes.AST_Node_Member_Reference'Class)
      return AST_Node_Member_Ref_Holder
      is
      begin
         return (Ref_Impl.Create (Value) with null record);
      end Create_Member_Ref;

      package Token_Impl is new Unbounded_Holders.Holders_Impl
        (AST_Token'Class,
         AST_Token_Access,
         AST_Token_Holders);

      -------------------
      -- Unchecked_Get --
      --------------------

      function Unchecked_Get
        (Self : AST_Token_Holder) return AST_Token_Access is
      begin
         return Token_Impl.Unchecked_Get (AST_Token_Holders.Holder (Self));
      end Unchecked_Get;

      ------------
      -- Create --
      ------------

      function Create_Token_Ref
        (Value : LKQL.AST_Nodes.AST_Token'Class)
      return AST_Token_Holder
      is
      begin
         return (Token_Impl.Create (Value) with null record);
      end Create_Token_Ref;

      package Unit_Impl is new Unbounded_Holders.Holders_Impl
        (AST_Unit'Class,
         AST_Unit_Access,
         AST_Unit_Holders);

      -------------------
      -- Unchecked_Get --
      --------------------

      function Unchecked_Get
        (Self : AST_Unit_Holder) return AST_Unit_Access is
      begin
         return Unit_Impl.Unchecked_Get (AST_Unit_Holders.Holder (Self));
      end Unchecked_Get;

      ------------
      -- Create --
      ------------

      function Create_Unit_Ref
        (Value : LKQL.AST_Nodes.AST_Unit'Class)
      return AST_Unit_Holder
      is
      begin
         return (Unit_Impl.Create (Value) with null record);
      end Create_Unit_Ref;

   end H;

   procedure Add_Children
     (Iter : in out Child_Iterator;
      Node : H.AST_Node_Holder);

   procedure Initialize_Next_Elements (Iter : in out Child_Iterator);

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter   : in out Child_Iterator;
      Result : out H.AST_Node_Holder) return Boolean
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

   procedure Add_Children
     (Iter : in out Child_Iterator;
      Node : H.AST_Node_Holder)
   is
   begin
      for I in reverse 1 .. Node.Unchecked_Get.Children_Count loop
         if not Node.Unchecked_Get.Nth_Child (I).Is_Null_Node then
            Iter.Next_Elements.Append
              (Create_Node (Node.Unchecked_Get.Nth_Child (I)));
         end if;
      end loop;
   end Add_Children;

   ------------------------------
   -- Initialize_Next_Elements --
   ------------------------------

   procedure Initialize_Next_Elements (Iter : in out Child_Iterator) is
   begin
      for El of Iter.Roots loop
         if not El.Unchecked_Get.Is_Null_Node then
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
         Result.Roots.Append (Node);
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

   -----------------
   -- Create_Node --
   -----------------

   function Create_Node
     (Value : LKQL.AST_Nodes.AST_Node'Class) return H.AST_Node_Holder is
   begin
      return H.Create_Node (Value);
   end Create_Node;

   -----------------------
   -- Create_Member_Ref --
   -----------------------

   function Create_Member_Ref
     (Value : LKQL.AST_Nodes.AST_Node_Member_Reference'Class)
      return H.AST_Node_Member_Ref_Holder
   is
   begin
      return H.Create_Member_Ref (Value);
   end Create_Member_Ref;

end LKQL.Partial_AST_Nodes;
