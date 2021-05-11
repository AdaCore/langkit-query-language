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

with LKQL.AST_Nodes;              use LKQL.AST_Nodes;
with LKQL.Depth_Nodes;            use LKQL.Depth_Nodes;
with LKQL.Selector_Lists;         use LKQL.Selector_Lists;
with LKQL.Eval_Contexts;          use LKQL.Eval_Contexts;
with LKQL.Partial_AST_Nodes;      use LKQL.Partial_AST_Nodes;

package LKQL.Patterns.Nodes is

   function Filter_Node_Array (Ctx     : Eval_Context;
                               Pattern : L.Base_Pattern;
                               Nodes   : AST_Node_Array)
                               return AST_Node_Array;
   --  Return a node array that only contains the nodes from 'Nodes' that match
   --  'Pattern'.

   function Match_Node_Pattern
     (Ctx     : Eval_Context;
      Pattern : L.Node_Pattern;
      Node    : H.AST_Node_Holder) return Match_Result;
   --  Match the given node against a node pattern

   function Match_Kind_pattern
     (Ctx     : Eval_Context;
      Pattern : L.Node_Kind_Pattern;
      Node    : H.AST_Node_Holder) return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match th given node against a kind pattern

   function Match_Extended_Pattern (Ctx     : Eval_Context;
                                    Pattern : L.Extended_Node_Pattern;
                                    Node    : H.AST_Node_Holder)
                                    return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match the given node against an extended pattern

   function Match_Pattern_Details (Ctx     : Eval_Context;
                                   Details : L.Node_Pattern_Detail_List;
                                   Node    : H.AST_Node_Holder)
                                   return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match a given node against the 'details' (fields, properties &
   --  selectors) of a node pattern.
   --  The 'Bindings' part of the Match result will contain references to the
   --  selector lists that are associated with a binding name in the pattern.

   function Match_Pattern_Detail (Ctx    : Eval_Context;
                                  Node   : H.AST_Node_Holder;
                                  Detail : L.Node_Pattern_Detail'Class)
                                  return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match 'Node' against a node pattern 'detail'

   function Match_Pattern_Field (Ctx    : Eval_Context;
                                 Node   : H.AST_Node_Holder;
                                 Field  : L.Node_Pattern_Field)
                                 return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match the expected value specified in 'Field' against the value of the
   --  field of 'Node' designated by 'Field'.

   function Match_Pattern_Property (Ctx      : Eval_Context;
                                    Node     : H.AST_Node_Holder;
                                    Property : L.Node_Pattern_Property)
                                    return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match the expected value specified in 'Property' against the value of
   --  the property call described in 'Property' on 'Node'.

   function Match_Pattern_Selector (Ctx      : Eval_Context;
                                    Node     : H.AST_Node_Holder;
                                    Selector : L.Node_Pattern_Selector)
                                    return Match_Result
     with Pre => not Node.Unchecked_Get.Is_Null_Node;
   --  Match 'Node' against a selector appearing as a node pattern detail.
   --  If the selector has a binding name, a binding associating the said name
   --  to the output of the selector will be added to the 'Bindings' part of
   --  the 'Match_Result'.

   function Eval_Selector (Ctx     : Eval_Context;
                           Node    : H.AST_Node_Holder;
                           Call    : L.Selector_Call;
                           Pattern : L.Base_Pattern;
                           Result  : out Selector_List) return Boolean;
   --  Return whether the evaluation of the given selector from 'Node' produces
   --  a valid result.
   --  If that is the case, the associated selector_list will be stored in
   --  'Result'.

   -----------------------------
   --  Node_Pattern_Predicate --
   -----------------------------

   type Node_Pattern_Predicate is new Depth_Node_Iters.Predicates.Func with
      record
         Ctx     : Eval_Context;
         Pattern : L.Base_Pattern;
      end record;
   --  Predicate that returns true when given a Depth_Node that matches
   --  'Pattern'.

   overriding function Evaluate
     (Self : in out Node_Pattern_Predicate; Node : Depth_Node) return Boolean;

   overriding function Clone
     (Self : Node_Pattern_Predicate) return Node_Pattern_Predicate;

   overriding procedure Release (Self : in out Node_Pattern_Predicate);

   function Make_Node_Pattern_Predicate (Ctx     : Eval_Context;
                                         Pattern : L.Base_Pattern)
                                         return Node_Pattern_Predicate;
   --  Create a Node_Pattern_Predicate with the given pattern and evaluation
   --  context.

private

   function Match_Detail_Value (Ctx    : Eval_Context;
                                Value  : Primitive;
                                Detail : L.Detail_Value) return Match_Result;

end LKQL.Patterns.Nodes;
