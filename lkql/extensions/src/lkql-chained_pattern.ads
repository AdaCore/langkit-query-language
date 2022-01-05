------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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
with LKQL.Patterns;      use LKQL.Patterns;
with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Doubly_Linked_Lists;

private package LKQL.Chained_Pattern is

   package Match_Result_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Match_Result);
   --  Lists of 'Match_Result' values

   subtype Match_Result_List is Match_Result_Lists.List;
   --  Lits of Match_Result values

   package Chained_Pattern_Iterators is
     new Iters.Iterators (Match_Result);
   --  Iterators of Match_Result values

   subtype Chained_Pattern_Iter
     is Chained_Pattern_Iterators.Iterator_Interface;
   --  Iterators of Match_Result values

   type Chained_Pattern_Iterator is new Chained_Pattern_Iter with private;
   --  Iterator that yields the values that belong to the result of a
   --  chained pattern.

   overriding function Next (Iter   : in out Chained_Pattern_Iterator;
                             Result : out Match_Result)
                             return Boolean;

   overriding function Clone (Iter : Chained_Pattern_Iterator)
                              return Chained_Pattern_Iterator;

   overriding procedure Release (Iter : in out Chained_Pattern_Iterator);

   function Make_Chained_Pattern_Iterator
     (Ctx           : Eval_Context;
      Root_Iterator : AST_Node_Iterator_Access;
      Pattern       : L.Chained_Node_Pattern) return Chained_Pattern_Iterator;
   --  Return an iterator that yields every node that matches the given
   --  chained pattern.

private

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => H.AST_Node_Holder,
      Hash                => H.Hash,
      Equivalent_Elements => H."=",
      "="                 => H."=");
   --  Sets of Ada nodes

   subtype Node_Set is Node_Sets.Set;
   --  Set of Ada nodes

   type Chained_Pattern_Iterator is new Chained_Pattern_Iter with record
      Ctx                 : Eval_Context;
      --  Context in which the patterns will be evaluated
      Next_Values         : Match_Result_List;
      --  Next values to be yielded, along with the bindings created while
      --  matching the sub patterns.
      Pattern             : L.Chained_Node_Pattern;
      --  THE pattern
      Root_Nodes_Iterator : AST_Node_Iterator_Access;
      --  Iterator that yields the nodes that match the first pattern of the
      --  chain.
      Yielded_Elements    : Node_Set;
      --  Cache storing the elements that have been yielded
   end record;

   procedure Eval_Element (Iter : in out Chained_Pattern_Iterator;
                           Root : H.AST_Node_Holder);
   --  Populate the 'Next_Values' list of 'Iter' by evaluating the pattern from
   --  'Root'.

   procedure Eval_Chain_From (Iter        : in out Chained_Pattern_Iterator;
                              Root        : H.AST_Node_Holder;
                              Link_Nb     : Positive);

   procedure Eval_Chain_From_Link
     (Iter        : in out Chained_Pattern_Iterator;
      Root        : H.AST_Node_Holder;
      Link_Nb     : Positive)
     with Pre => Link_Nb <= Iter.Pattern.F_Chain.Children_Count;

   function Eval_Link (Ctx             : Eval_Context;
                       Root            : H.AST_Node_Holder;
                       Link            : L.Chained_Pattern_Link)
                       return AST_Node_Vector;
   --  Return the result of a link's evaluation.
   --  If the link introduces new bindings, they will be added to 'Bindings'.
   --  If 'Link' is a selector link, the related pattern is used to verify the
   --  quantifier.

   function Eval_Selector_Link (Ctx             : Eval_Context;
                                Root            : H.AST_Node_Holder;
                                Selector        : L.Selector_Link)
                                return AST_Node_Vector;

   function Eval_Field_Link (Ctx   : Eval_Context;
                             Root  : H.AST_Node_Holder;
                             Field : L.Field_Link)
                             return AST_Node_Vector;

   function Eval_Property_Link (Ctx : Eval_Context;
                                Root : H.AST_Node_Holder;
                                Property : L.Property_Link)
                                return AST_Node_Vector;

   function To_AST_Node_Vector (Value : Primitive) return AST_Node_Vector;

end LKQL.Chained_Pattern;
