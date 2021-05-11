------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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

with Ada.Unchecked_Deallocation;

limited with LKQL.AST_Nodes;
with Unbounded_Holders;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Iters.Iterators;

--  This package gives a partial view on AST nodes and related API, via holder.
--  It exposes the ``AST_Node_Holder`` type, which is the type that is globally
--  used in LKQL to store AST Nodes. It only uses a partial view of the
--  ``AST_Nodes`` package, which allows to break a circular dependency between
--  ``LKQL.Primitives`` and ``LKQL.AST_Nodes``.
--
--  It exposes all non-core AST node related APIS, such as node lists, vectors,
--  arrays, and iterators.

package LKQL.Partial_AST_Nodes is

   --  This package exposes two holder types, ``AST_Node_Holder`` and
   --  ``AST_Node_Member_Ref_Holder``, which allow to store those types with
   --  only a partial view on ``LKQL.AST_Nodes``.
   --
   --  They're enclosed in a package so that some operations on iterators are
   --  not tagged as primitives, and so that we can declare list/vectors of
   --  them in the same package while keeping them private.
   package H is
      type AST_Node_Holder is tagged private;
      --  Holder for an ``AST_node``.
      --
      --  .. attention:: This is the type that you are supposed to use
      --    **everywhere** in LKQL to store AST Nodes.

      type AST_Node_Access is access all LKQL.AST_Nodes.AST_Node'Class;
      --  Access to an ``AST_node``.
      --
      --  .. attention:: You should **NOT** use this type to store AST node
      --    references.

      function Unchecked_Get (Self : AST_Node_Holder) return AST_Node_Access;
      --  Return an access to the node stored in ``Self``

      function Create_Node
        (Value : LKQL.AST_Nodes.AST_Node'Class) return AST_Node_Holder;
      --  Create a node holder from the given ``Value``

      function "=" (L, R : AST_Node_Holder) return Boolean;
      function Hash (Self : AST_Node_Holder) return Hash_Type;

      type AST_Node_Holder_Array
      is array (Positive range <>) of AST_Node_Holder;
      --  Array of ``AST_Node``

      type AST_Node_Member_Ref_Holder is tagged private;
      --  Holder for an ``AST_Node_Member_Reference``

      type AST_Node_Member_Ref_Access
      is access all LKQL.AST_Nodes.AST_Node_Member_Reference'Class;
      --  Access to an ``AST_Node_Member_Ref``

      function Unchecked_Get
        (Self : AST_Node_Member_Ref_Holder) return AST_Node_Member_Ref_Access;
      --  Return an access to the member ref stored in ``Self``

      function Create_Member_Ref
        (Value : LKQL.AST_Nodes.AST_Node_Member_Reference'Class)
      return AST_Node_Member_Ref_Holder;
      --  Create a member ref holder from the given ``Value``
   private

      package Holders is new Unbounded_Holders.Base_Holders
        (256);
      type AST_Node_Holder is new Holders.Holder with null record;
      type AST_Node_Member_Ref_Holder is new Holders.Holder with null record;
   end H;

   function Create_Node
     (Value : LKQL.AST_Nodes.AST_Node'Class) return H.AST_Node_Holder;
   --  Shortcut to H.Create_Node

   function Create_Member_Ref
     (Value : LKQL.AST_Nodes.AST_Node_Member_Reference'Class)
      return H.AST_Node_Member_Ref_Holder;
   --  Shortcut to H.Create_Member_Ref

   type AST_Node_Array is array (Positive range <>) of H.AST_Node_Holder;
   --  Array of AST node pointers

   Empty_Ast_Node_Array : constant AST_Node_Array (1 .. 0) := (others => <>);
   --  Empty node array constant

   type AST_Node_Array_Access is access all AST_Node_Array;
   --  Pointer to an array of AST node pointers

   procedure Free_Ast_Node_Array is new Ada.Unchecked_Deallocation
     (AST_Node_Array, AST_Node_Array_Access);
   --  Free a pointer to an array of AST node pointers

   package AST_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => H.AST_Node_Holder,
        "="          => H."=");
   --  Doubly-linked lists of refcounted AST node pointers

   subtype AST_Node_List is AST_Node_Lists.List;
   --  Doubly-linked list of refcounted AST node pointers

   package AST_Node_Vectors is new Ada.Containers.Vectors
     (Element_Type => H.AST_Node_Holder,
      Index_Type   => Positive,
      "="          => H."=");
   --  Vectors of refcounted AST node pointers

   subtype AST_Node_Vector is AST_Node_Vectors.Vector;
   --  Vector of refcounted AST node pointers

   package AST_Node_Iterators is new Iters.Iterators (H.AST_Node_Holder);
   --  Iterators of refcounted AST node pointers

   subtype AST_Node_Iterator is AST_Node_Iterators.Iterator_Interface;
   --  Iterator of refcounted AST node pointers

   subtype AST_Node_Iterator_Access is AST_Node_Iterators.Iterator_Access;
   --  Pointer to an iterator of refcounted AST node pointers

   subtype AST_Node_Iterator_Predicate is AST_Node_Iterators.Predicates.Func;
   --  Predicate on refcounted AST node pointers

   subtype AST_Node_Predicate_Access is
     AST_Node_Iterators.Predicates.Func_Access;
   --  Pointer to a predicate on AST node pointers

   --------------------
   -- Child_Iterator --
   --------------------

   type Child_Iterator is new AST_Node_Iterator with private;
   --  Iterator that yields the children of a node in a depth-first fashion

   overriding function Next
     (Iter   : in out Child_Iterator;
      Result : out H.AST_Node_Holder) return Boolean;

   overriding function Clone (Iter : Child_Iterator) return Child_Iterator;

   function Make_Child_Iterator (Nodes : AST_Node_Array) return Child_Iterator;
   function Make_Child_Iterator
     (Nodes : AST_Node_Vector) return Child_Iterator;

private

   type Child_Iterator is new AST_Node_Iterator with record
      Roots         : AST_Node_Vector;
      Next_Elements : AST_Node_List;
   end record;

end LKQL.Partial_AST_Nodes;
