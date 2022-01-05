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

with LKQL.Depth_Nodes;   use LKQL.Depth_Nodes;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;
with LKQL.Primitives; use LKQL.Primitives;

private package LKQL.Custom_Selectors is

   type Custom_Selector_Iter is new Depth_Node_Iter with private;
   --  Iterator that yields the nodes produced by a custom selector

   function Next
     (Iter   : in out Custom_Selector_Iter;
      Result : out Depth_Node) return Boolean;

   function Clone (Iter : Custom_Selector_Iter) return Custom_Selector_Iter;

   function Make_Custom_Selector_Iter
     (Ctx                            : Eval_Context;
      Selector                       : Primitive;
      Min_Depth_Expr, Max_Depth_Expr : L.Expr;
      Root                           : H.AST_Node_Holder)
      return Custom_Selector_Iter;
   --  Create an iterator that yields the nodes bound to 'Root' by
   --  the given selector definition.

private

   package Depth_Node_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Depth_Node);
   --  List of Depth_Node values

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => H.AST_Node_Holder,
      Hash                => H.Hash,
      Equivalent_Elements => H."=",
      "="                 => H."=");
   --  Set of Depth_Node values

   type Custom_Selector_Iter is new Depth_Node_Iter with record
      Ctx             : Eval_Context;
      --  Copy of the evaluation context

      Selector        : Primitive;
      --  LKQL definition of the custom selector

      Min_Depth       : Integer;
      --  Minimum depth of the nodes. If Min_Depth < 0, the minimum depth will
      --  be ignored.

      Max_Depth       : Integer;
      --  Maximum depth of the nodes. If Max_Depth < 0, the maximum depth will
      --  be ignored.

      Next_Values     : Depth_Node_Lists.List;
      --  Nodes that will be yielded

      Next_To_Visit   : Depth_Node_Lists.List;
      --  Nodes that will be used as an evaluation root for the selector in
      --  order to find new nodes to yield.

      Already_Yielded : Node_Sets.Set;
      --  Nodes that have already been yielded

      Already_Visited : Node_Sets.Set;
      --  Nodes that have already been visited
   end record;

   procedure Eval_Selector (Iter : in out Custom_Selector_Iter);
   --  Remove the first value from the 'Next_To_Visit' list and use it as the
   --  root for the selector's evaluation.
   --  The 'Next_Values' and 'Next_To_Visit' lists will be updated with the
   --  values produced by the selector.

   procedure Eval_Selector (Iter : in out Custom_Selector_Iter;
                            Node : Depth_Node);
   --  Remove the first value from the 'Next_To_Visit' list and use it as the
   --  root for the selector's evaluation.
   --  The 'Next_Values' and 'Next_To_Visit' lists will be updated with the
   --  values produced by the selector.

end LKQL.Custom_Selectors;
