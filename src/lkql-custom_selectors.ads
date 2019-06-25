with LKQL.AST_Nodes;     use LKQL.AST_Nodes;
with LKQL.Depth_Nodes;   use LKQL.Depth_Nodes;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Sets;

private package LKQL.Custom_Selectors is

   type Custom_Selector_Iter is new Depth_Node_Iter with private;
   --  Iterator that yields the nodes produced by a custom selector

   function Next (Iter   : in out Custom_Selector_Iter;
                  Result : out Depth_Node) return Boolean;

   function Clone (Iter : Custom_Selector_Iter) return Custom_Selector_Iter;

   function Make_Custom_Selector_Iter (Ctx        : Eval_Context;
                                       Call       : L.Selector_Call;
                                       Root       : AST_Node_Rc)
                                       return Custom_Selector_Iter;
   --  Create an iterator that yields the nodes bound to 'Root' by
   --  the given selector defitnion.

private

   package Depth_Node_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Depth_Node);
   --  List of Depth_Node values

   package Depth_Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Depth_Node,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");
   --  Set of Depth_Node values

   type Custom_Selector_Iter is new Depth_Node_Iter with record
      Ctx             : Eval_Context;
      --  Copy of the evaluation context
      Selector        : L.Selector_Decl;
      --  LKQL definititon of the custom selector
      Min_Depth       : Integer;
      --  Minimum depth of the nodes. If Min_Depth < 0, the minimum depth will
      --  be ingored.
      Max_Depth       : Integer;
      --  Maximum depth of the nodes. If Max_Depth < 0, the maximum depth will
      --  be ignored.
      Next_Values     : Depth_Node_Lists.List;
      --  Nodes that will be yielded
      Next_To_Visit   : Depth_Node_Lists.List;
      --  Nodes that will be used as an evaluation root for the selector in
      --  order to find new nodes to yield.
      Already_Yielded : Depth_Node_Sets.Set;
      --  Nodes that have already been yielded
      Already_Visited : Depth_Node_Sets.Set;
      --  Nodes that have laready been visited
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

   procedure Add_Selector_Expr (Iter      : in out Custom_Selector_Iter;
                                Local_Ctx : Eval_Context;
                                Depth     : Natural;
                                Expr      : L.Selector_Expr);
   --  Add the result of 'Expr's evaluation to the values produced by the
   --  selector.

   procedure Add_Node (Iter          : in out Custom_Selector_Iter;
                       Current_Depth : Natural;
                       Node          : AST_Node_Rc;
                       Mode          : L.Selector_Expr_Mode);
   --  Add the given node to the values produced by the selector.
   --  The value will be added to 'Next_Values' or 'Next_To_Visit' (or both)
   --  depending on the given mode.

   procedure Add_If_Unseen
     (Node        : Depth_Node;
      Cache       : in out Depth_Node_Sets.Set;
      Target_List : out Depth_Node_Lists.List);
   --  Add 'Node' to the target list if it's node value is not already in the
   --  cache, and cache it.

end LKQL.Custom_Selectors;
