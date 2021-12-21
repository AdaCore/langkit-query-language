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

--  This package gives a partial view on Langkit-frontend specific data, via
--  holder types.
--
--  It exposes the ``AST_Node_Holder``, ``AST_Node_Member_Ref_Holder``,
--  ``AST_Unit_Holder`` and ``AST_Token_Holder`` types, which are the types
--  that are globally used in LKQL to store language specific data like
--  nodes/tokens/units/etc. It only uses a partial view of the ``AST_Nodes``
--  package, which allows to break a circular dependency between
--  ``LKQL.Primitives`` and ``LKQL.AST_Nodes``.
--
--  Using holder types allows us to have definite types to store language
--  specific data in LKQL, instead of having to allocate data on the heap via
--  ``new`` or controlled objects, which is good for performance.
--
--  It exposes all non-core AST node related APIS, such as node lists, vectors,
--  arrays, and iterators.

package LKQL.Partial_AST_Nodes is

   --  This package exposes all the holder types, which allow to store those
   --  types with only a partial view on ``LKQL.AST_Nodes``.
   --
   --  They're enclosed in a package so that some operations on iterators are
   --  not tagged as primitives, and so that we can declare list/vectors of
   --  them in the same package while keeping them private.
   package H is

      ---------------------
      -- AST_Node_Holder --
      ---------------------

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

      -------------------------
      -- AST_Node_Member_Ref --
      -------------------------

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

      ----------------------
      -- AST_Token_Holder --
      ----------------------

      type AST_Token_Holder is tagged private;
      type AST_Token_Access is access all LKQL.AST_Nodes.AST_Token'Class;

      function Unchecked_Get (Self : AST_Token_Holder) return AST_Token_Access;
      function Create_Token_Ref
        (Value : LKQL.AST_Nodes.AST_Token'Class) return AST_Token_Holder;

      ---------------------
      -- AST_Unit_Holder --
      ---------------------

      type AST_Unit_Holder is tagged private;
      type AST_Unit_Access is access all LKQL.AST_Nodes.AST_Unit'Class;

      function Unchecked_Get
        (Self : AST_Unit_Holder) return AST_Unit_Access;
      function Create_Unit_Ref
        (Value : LKQL.AST_Nodes.AST_Unit'Class) return AST_Unit_Holder;
      function Hash (Self : AST_Unit_Holder) return Hash_Type;

   private

      --  All those holders hold classwide instance of types from the
      --  LKQL.AST_Nodes hierarchy. Those types are tagged for dispatch
      --  purposes, so that we can one day plug another front-end than LAL
      --  here, but they will always have the same representation and size,
      --  because they encapsulate pointer-like data that will have the same
      --  size in every Langkit front-end.
      --
      --  So we use holders of the exact needed size for each one of those. We
      --  cannot know/infer the size statically from here, which is why the
      --  below numbers are hardcoded.
      --
      --  If the size of the encapsulated data was to ever change and get
      --  bigger, we would get errors from the implementation of holders,
      --  signaling us that we need to update the below numbers.

      package AST_Node_Holders is new Unbounded_Holders.Base_Holders (104);
      package AST_Unit_Holders is new Unbounded_Holders.Base_Holders (40);
      package AST_Token_Holders is new Unbounded_Holders.Base_Holders (80);
      package AST_Node_Member_Ref_Holders
      is new Unbounded_Holders.Base_Holders (16);

      type AST_Node_Holder is new AST_Node_Holders.Holder with null record;

      type AST_Node_Member_Ref_Holder
      is new AST_Node_Member_Ref_Holders.Holder with null record;

      type AST_Token_Holder is new AST_Token_Holders.Holder with null record;

      type AST_Unit_Holder is new AST_Unit_Holders.Holder with null record;
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

   ------------------------
   -- AST Node Iterators --
   ------------------------

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