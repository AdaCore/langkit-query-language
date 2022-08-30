------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Libadalang.Generic_API; use Libadalang.Generic_API;
with Libadalang.Generic_API.Introspection;
use Libadalang.Generic_API.Introspection;

package body LKQL.Lk_Nodes_Iterators is

   procedure Add_Children
     (Iter : in out Child_Iterator;
      Node : LK.Lk_Node);

   procedure Initialize_Next_Elements (Iter : in out Child_Iterator);

   -------------------------
   -- Make_Child_Iterator --
   -------------------------

   function Make_Child_Iterator
     (Nodes : Lk_Node_Array) return Child_Iterator
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
     (Nodes : Lk_Node_Vector;
      Follow_Instantiations : Boolean := False) return Child_Iterator
   is
      Result       : Child_Iterator;
   begin
      Result.Roots := Nodes;
      Result.Follow_Instantiations := Follow_Instantiations;
      Initialize_Next_Elements (Result);
      return Result;
   end Make_Child_Iterator;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter   : in out Child_Iterator;
      Result : out Lk_Node) return Boolean
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

   Designated_Generic_Decl : constant LKI.Struct_Member_Ref :=
     Member_Refs.Generic_Instantiation_P_Designated_Generic_Decl;

   Body_Part_For_Decl : constant LKI.Struct_Member_Ref :=
     Member_Refs.Basic_Decl_P_Body_Part_For_Decl;

   procedure Add_Children
     (Iter : in out Child_Iterator;
      Node : Lk_Node)
   is
   begin
      for I in reverse 1 .. Node.Children_Count loop
         if not Node.Child (I).Is_Null then
            Iter.Next_Elements.Append (Node.Child (I));
         end if;
      end loop;

      if
         Iter.Follow_Instantiations
         and then LKI.Type_Matches (Node, Type_Refs.Generic_Instantiation)
      then
         declare
            Gen_Decl : constant LK.Lk_Node := LKI.As_Node
              (LKI.Eval_Node_Member
                (Node, Designated_Generic_Decl));

            Gen_Body : constant LK.Lk_Node := LKI.As_Node
              (LKI.Eval_Node_Member
                (Gen_Decl,
                 Body_Part_For_Decl,
                 (1 => LKI.From_Bool (Ada_Lang_Id, False))));
         begin
            Iter.Next_Elements.Append (Gen_Decl);
            if not Gen_Body.Is_Null then
               Iter.Next_Elements.Append (Gen_Body);
            end if;
         end;
      end if;
   end Add_Children;

   ------------------------------
   -- Initialize_Next_Elements --
   ------------------------------

   procedure Initialize_Next_Elements (Iter : in out Child_Iterator) is
   begin
      for El of Iter.Roots loop
         if not El.Is_Null then
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

end LKQL.Lk_Nodes_Iterators;
