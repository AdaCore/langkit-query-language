------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                    Copyright (C) 2022-2023, AdaCore                      --
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

with Iters.Iterators;
with Ada.Containers.Doubly_Linked_Lists;

package LKQL.Lk_Nodes_Iterators is

   ------------------------
   -- AST Node Iterators --
   ------------------------

   package Lk_Node_Iterators is new Iters.Iterators (Lk_Node);
   --  Iterators of refcounted AST node pointers

   subtype Lk_Node_Iterator is Lk_Node_Iterators.Iterator_Interface;
   --  Iterator of refcounted AST node pointers

   subtype Lk_Node_Iterator_Access is Lk_Node_Iterators.Iterator_Access;
   --  Pointer to an iterator of refcounted AST node pointers

   subtype Lk_Node_Iterator_Predicate is Lk_Node_Iterators.Predicates.Func;
   --  Predicate on refcounted AST node pointers

   subtype Lk_Node_Predicate_Access is
     Lk_Node_Iterators.Predicates.Func_Access;
   --  Pointer to a predicate on AST node pointers

   --------------------
   -- Child_Iterator --
   --------------------

   type Child_Iterator is new Lk_Node_Iterator with private;
   --  Iterator that yields the children of a node in a depth-first fashion

   overriding function Next
     (Iter   : in out Child_Iterator;
      Result : out Lk_Node) return Boolean;

   overriding function Clone (Iter : Child_Iterator) return Child_Iterator;

   function Make_Child_Iterator (Nodes : Lk_Node_Array) return Child_Iterator;
   function Make_Child_Iterator
     (Nodes : Lk_Node_Vector;
      Follow_Instantiations : Boolean := False) return Child_Iterator;

private

   package AST_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => Lk_Node,
        "="          => "=");
   --  Doubly-linked lists of refcounted AST node pointers

   subtype AST_Node_List is AST_Node_Lists.List;
   --  Doubly-linked list of refcounted AST node pointers

   type Child_Iterator is new Lk_Node_Iterator with record
      Roots                 : Lk_Node_Vector;
      Next_Elements         : AST_Node_List;
      Follow_Instantiations : Boolean := False;
   end record;

end LKQL.Lk_Nodes_Iterators;
