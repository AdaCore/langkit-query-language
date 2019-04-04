with Query.Iterators; use Query.Iterators;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

package Query.Patterns is

   subtype Iterator_Predicate_Interface is Node_Iterators.Predicates.Func;
   --  Predicates on 'Iterator_Node' values

   subtype Iterator_Predicate_Access is Node_Iterators.Predicate_Access;
   --  Pointer to a predicate on 'Iterator_Node' values

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  belong to the result of the given query.

private

   ---------------------
   -- Query_Predicate --
   ---------------------

   type Query_Predicate is new Iterator_Predicate_Interface with record
      Ctx   : Eval_Context_Ptr;
      Query : LEL.Query;
   end record;
   --  Predicate that returns true for every node that belongs to the
   --  result of the given query.

   type Query_Predicate_Access is access all Query_Predicate;
   --  Pointer to a Query_predicate

   function Make_Query_Predicate
     (Ctx : Eval_Context_Ptr; Query : LEL.Query) return Query_Predicate_Access;
   --  Return a pointer to a Query_Predicate that returns true for every node
   --  that belongs to the result set of the given query.

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : Iterator_Node) return Boolean;
   --  Evaluae the given predicate against 'Node'

   overriding function Clone
     (Self : Query_Predicate) return Query_Predicate;
   --  Return a copy of the given Query_Predicate

   --------------
   -- Matching --
   --------------

   function Match_Unfiltered_Query (Ctx   : Eval_Context_Ptr;
                                    Query : LEL.Query;
                                    Node  : Iterator_Node) return Boolean;
   --  Given a query of the form "query PATTERN", return true if 'Node' belongs
   --  to the result set of the query.

   function Match_Filtered_Query (Ctx   : Eval_Context_Ptr;
                                  Query : LEL.Filtered_Query;
                                  Node  : Iterator_Node) return Boolean;
   --  Given a query of the form "query PATTERN when CONDITON", return true if
   --  'Node' belongs to the result set of the query.

   type Match is record
      Success : Boolean;
      --  True if the match attempt succeeded
      Bindings : String_Value_Maps.Map;
      --  Stores the bindings specified in the matched pattern, if any
   end record;
   --  Represents the result of a match attempt

   Match_Failure : constant Match :=
     (Success => False, Bindings => String_Value_Maps.Empty_Map);

   function Match_Query_Pattern (Ctx           : Eval_Context_Ptr;
                                 Query_Pattern : LEL.Query_Pattern;
                                 Node          : Iterator_Node)
                                 return Match;
   --  Match a query pattern

   function Match_Full_Query_Pattern
     (Ctx           : Eval_Context_Ptr;
      Query_Pattern : LEL.Full_Query_Pattern;
      Node          : Iterator_Node) return Match;
   --  Match a query pattern of the form: QUERIED_NODE [SELECTOR] RELATED_NODE

   function Match_Node_Pattern
     (Node_Pattern : LEL.Node_Pattern; Node : LAL.Ada_Node) return Match;
   --  Match a node pattern

   function Match_Binding_Node_Pattern
     (Node_Pattern : LEL.Binding_Node_Pattern;
      Node         : LAL.Ada_Node) return Match;
   --  Match a node pattern consisting of a binding name

   function Match_Kind_Node_Pattern
     (Node_Pattern : LEL.Kind_Node_Pattern; Node : LAL.Ada_Node) return Match;
   --  Match a node pattern consisting of a kind name

   function Match_Full_Node_Pattern (Node_Pattern : LEL.Full_Node_Pattern;
                                     Node : LAL.Ada_Node) return Match;
   --  Match a node pattern of the form: BINDING @ KIND_NAME

   function Make_Selector_Iterator
     (Ctx              : Eval_Context_Ptr;
      Queried_Node     : Iterator_Node;
      Selector_Pattern : LEL.Selector_Pattern'Class)
      return Node_Iterator'Class;
   --  Return an iterator that yields the nodes bound to Queried_Node by the
   --  given selector.

   function Selector_Iterator_From_Name
     (Ctx           : Eval_Context_Ptr;
      Queried_Node  : LAL.Ada_Node;
      Selector_Pattern : LEL.Selector_Pattern'Class)
      return Node_Iterator'Class;
   --  Given a selector name N (ex: 'children'), return a Node iterator that
   --  yields all the nodes bound to 'Queried_Node' by N.

   ------------------------
   -- Selector consumers --
   ------------------------

   package Selector_Consumers is new Node_Iterators.Consumers (Match);
   --  Consummer for Iterators over 'Iterator_Node' values that produces
   --  'Match' values as a result.

   subtype Node_Consumer is Selector_Consumers.Consumer_Interface;

   type Exists_Consumer is new Selector_Consumers.Consumer_Interface
   with record
      Pattern : LEL.Node_Pattern;
   end record;
   --  Consumer that produces a successful 'Match' value if at least one of
   --  the values yieded by the consumed iterator matches 'Pattern'.

   function Consume (Self : in out Exists_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match;

   type All_Consumer is new Selector_Consumers.Consumer_Interface
   with record
      Pattern : LEL.Node_Pattern;
   end record;
   --  Consumer that produces a successful 'Match' value if all of the values
   --  yielded by the consumed iterator match 'Pattern', or the iterator
   --  doesn't yield any value.

   function Consume (Self : in out All_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match;

   function Make_Selector_Consumer (Selector     : LEL.Selector_Pattern;
                                    Related_Node : LEL.Node_Pattern)
                                    return Node_Consumer'Class;
   --  Given a selector pattern, return a selector consumer that corresponds
   --  to the quantifier specified in the pattern.

   type Selector_Conditions_Predicate is new Iterator_Predicate_Interface
   with record
      Context   : Eval_Context_Ptr;
      Condition : LEL.Expr;
   end record;
   --  Predicate that evaluates a selector's filtering expression

   overriding function Evaluate
     (Self    : in out Selector_Conditions_Predicate;
      Element : Iterator_Node)
      return Boolean;

   overriding function Clone (Self : Selector_Conditions_Predicate)
                              return Selector_Conditions_Predicate;
   --  Return a copy of the given Selector_Conditions_Predicate

end Query.Patterns;
