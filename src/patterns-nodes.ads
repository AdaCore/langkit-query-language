with Options;
with Depth_Nodes;            use Depth_Nodes;
with Interpreter.Primitives; use Interpreter.Primitives;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package Patterns.Nodes is

   function Match_Node (Ctx     : Eval_Context;
                        Pattern : L.Node_Pattern;
                        Value   : Primitive) return Match_Result;
   --  Return the result of matching 'Value' against the given node pattern

   function Matches_Kind_Name
     (Kind_Name : String; Node : LAL.Ada_Node) return Boolean;
   --  Return true if 'Node's type is named 'Type_Name' or is a subtype of
   --  a type named 'Type_Name'.

   -------------------------------------
   -- Selectors creation & evaluation --
   -------------------------------------

   function Make_Selector_Iterator
     (Ctx              : Eval_Context;
      Queried_Node     : LAL.Ada_Node'Class;
      Selector_Pattern : L.Selector_Pattern'Class)
      return Depth_Node_Iter'Class;
   --  Return an iterator that yields the nodes bound to Queried_Node by the
   --  given selector.

   function Selector_Iterator_From_Name
     (Ctx              : Eval_Context;
      Queried_Node     : LAL.Ada_Node;
      Selector_Pattern : L.Selector_Pattern'Class)
      return Depth_Node_Iter'Class;

   ------------------------
   -- Selector consumers --
   ------------------------

   package Selector_Consumers is new Depth_Node_Iters.Consumers (Match_Result);
   --  Consummer for Iterators over 'Depth_Node' values that produces
   --  'Match' values as a result.

   subtype Node_Consumer is Selector_Consumers.Consumer_Interface;

   type Exists_Consumer is new Node_Consumer
   with record
      Ctx     : Eval_Context;
      Pattern : L.Unfiltered_Pattern;
   end record;
   --  Consumer that produces a successful 'Match' value if at least one of
   --  the values yieded by the consumed iterator matches 'Pattern'.

   function Consume (Self : in out Exists_Consumer;
                     Iter : in out Depth_Node_Iter'Class)
                     return Match_Result;

   type All_Consumer is new Node_Consumer
   with record
      Ctx     : Eval_Context;
      Pattern : L.Unfiltered_Pattern;
   end record;
   --  Consumer that produces a successful 'Match' value if all of the values
   --  yielded by the consumed iterator match 'Pattern', or the iterator
   --  doesn't yield any value.

   function Consume (Self : in out All_Consumer;
                     Iter : in out Depth_Node_Iter'Class)
                     return Match_Result;

   function Make_Selector_Consumer (Ctx          : Eval_Context;
                                    Selector     : L.Selector_Pattern;
                                    Related_Node : L.Unfiltered_Pattern)
                                    return Node_Consumer'Class;
   --  Given a selector pattern, return a selector consumer that corresponds
   --  to the quantifier specified in the pattern.

   subtype Iterator_Predicate_Interface is Depth_Node_Iters.Predicates.Func;
   --  Predicates on 'Depth_Node' values

   type Selector_Conditions_Predicate is new Iterator_Predicate_Interface
   with record
      Context   : Eval_Context;
      Condition : L.Expr;
   end record;
   --  Predicate that evaluates a selector's filtering expression

   overriding function Evaluate
     (Self    : in out Selector_Conditions_Predicate;
      Element : Depth_Node)
      return Boolean;

   overriding function Clone (Self : Selector_Conditions_Predicate)
                              return Selector_Conditions_Predicate;
   --  Return a copy of the given Selector_Conditions_Predicate

   ---------------------
   -- Childs_Iterator --
   ---------------------

   package Depth_Node_Options is new Options (Depth_Node);
   use Depth_Node_Options;
   --  Optional Depth_Node values

   package Depth_Node_Vectors is new Ada.Containers.Vectors
     (Positive, Depth_Node);
   --  Vector of Depth_Node values

   type Element_Vector_Access is access all Depth_Node_Vectors.Vector;
   --  Pointer to a Vector if Depth_Node values

   procedure Free_Element_Vector is new Ada.Unchecked_Deallocation
     (Depth_Node_Vectors.Vector, Element_Vector_Access);

   type Childs_Iterator is new Depth_Node_Iter with record
      Stack : Element_Vector_Access;
   end record;
   --  Iterator that yields the children of a given node

   overriding function Next (Iter : in out Childs_Iterator;
                             Element : out Depth_Node)
                             return Boolean;
   --  Get the next children. If there is no children to yield
   --  anymore, return False. Otherwise, return True and set 'Result'.

   overriding procedure Release (Iter : in out Childs_Iterator);
   --  Release ressources that belong to 'Iter'

   overriding function Clone (Iter : Childs_Iterator) return Childs_Iterator;

   function Make_Childs_Iterator (Node : LAL.Ada_Node) return Childs_Iterator;
   --  Return an iterator that yields all nodes under 'Node' (included) in
   --  a depth first search fashion.

   procedure Stack_Childs
     (Iter : in out Childs_Iterator; Element : Depth_Node);
   --  Push the children of 'Element' onto 'Iter's stack

   function Pop
     (Stack  : Element_Vector_Access) return Option;
   --  Pop an element from 'Stack'

end Patterns.Nodes;
