with Iters.Iterators;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Common;
with Liblkqllang.Analysis;

with Libadalang.Common;
with Libadalang.Analysis;
with Libadalang.Iterators;

with Ada.Unchecked_Deallocation;

package Query is
   package LEL renames Liblkqllang.Analysis;
   package LELCO renames Liblkqllang.Common;
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   package Node_Iterators is new Iters.Iterators (LAL.Ada_Node);
   --  Iterator over a sequence of Ada_Node values

   subtype Node_Iterator_Access is Node_Iterators.Iterator_Access;
   --  Raw pointer to an iterator over a sequence of Ada_Node values

   subtype Node_Iterator is Node_Iterators.Iterator_Interface;
   --  Interface implemented by iterators over Ada_Node values

   subtype Node_Predicate_Interface is Node_Iterators.Predicates.Func;
   --  Interface implemented by predicates that take an Ada_Node argument

   subtype Node_Predicate_Access is Node_Iterators.Predicate_Access;

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  belong to the result of the given query.

private

   type Query_Predicate is new Node_Predicate_Interface with record
      Ctx : Eval_Context_Ptr;
      Query : LEL.Query;
   end record;
   --  Node predicate that returns true for every node that belongs to the
   --  result of the given query.

   type Query_Predicate_Access is access all Query_Predicate;
   --  Pointer to a Query_predicate

   function Make_Query_Predicate
     (Ctx : Eval_Context_Ptr; Query : LEL.Query) return Query_Predicate_Access;

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : LAL.Ada_Node) return Boolean;

   function Match_Unfiltered_Query (Ctx   : Eval_Context_Ptr;
                                    Query : LEL.Query;
                                    Node  : LAL.Ada_Node) return Boolean;
   --  Given a query of the form "query PATTERN", return true if 'Node' belongs
   --  to the result set of the query.

   function Match_Filtered_Query (Ctx   : Eval_Context_Ptr;
                                  Query : LEL.Filtered_Query;
                                  Node  : LAL.Ada_Node) return Boolean;
   --  Given a query of the form "query PATTERN when CONDITON", return true if
   --  'Node' belongs to the result set of the query.

   function Backup_Env (Parent_Env  : String_Value_Maps.Map;
                        Local_Env  : String_Value_Maps.Map)
                        return String_Value_Maps.Map;
   --  Return the key-value pairs from Parent_Env which have a key that belongs
   --  to the key set of Local_Env.

   procedure Update_Env (Env        : in out String_Value_Maps.Map;
                         New_Values : String_Value_Maps.Map);
   --  Insert all the key-value pairs from New_Values into Env. In case of
   --  a conflict, the value from Env will be overriden.

   type Match is record
      Success : Boolean;
      Bindings : String_Value_Maps.Map;
   end record;

   Match_Failure : constant Match :=
     (Success => False, Bindings => String_Value_Maps.Empty_Map);

   function Match_Query_Pattern (Ctx           : Eval_Context_Ptr;
                                 Query_Pattern : LEL.Query_Pattern;
                                 Node          : LAL.Ada_Node) return Match;
   --  Match a query pattern

   function Match_Full_Query_Pattern
     (Ctx           : Eval_Context_Ptr;
      Query_Pattern : LEL.Full_Query_Pattern;
      Node          : LAL.Ada_Node) return Match;
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
      Queried_Node     : LAL.Ada_Node;
      Selector_Pattern : LEL.Selector_Pattern'Class)
      return Node_Iterator'Class;
   --  Return an iterator that yields the nodes bound to Queried_Node by the
   --  given selector.

   type Traverse_Iterator_Access is
     access all Libadalang.Iterators.Traverse_Iterator'Class;

   procedure Free_Traverse_Iterator is new Ada.Unchecked_Deallocation
     (Libadalang.Iterators.Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Traverse_Wrapper is new Node_Iterator with record
      Inner : Traverse_Iterator_Access;
   end record;
   --  Iterator that wrapps a Libadalang Traverse iterator

   overriding function Next (Iter : in out Traverse_Wrapper;
                             Result : out LAL.Ada_Node) return Boolean;

   overriding procedure Release (Iter : in out Traverse_Wrapper);

   function Make_Travers_Wrapper
     (Root : LAL.Ada_Node) return Traverse_Wrapper;

   ------------------------
   -- Selector consumers --
   ------------------------

   package Selector_Consumers is new Node_Iterators.Consumers (Match);

   subtype Node_Consumer is Selector_Consumers.Consumer_Interface;

   type Exists_Consumer is new Selector_Consumers.Consumer_Interface
   with record
      Pattern : LEL.Node_Pattern;
   end record;

   function Consume (Self : in out Exists_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match;

   type All_Consumer is new Selector_Consumers.Consumer_Interface
   with record
      Pattern : LEL.Node_Pattern;
   end record;

   function Consume (Self : in out All_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match;

   function Make_Selector_Consumer (Selector     : LEL.Selector_Pattern;
                                    Related_Node : LEL.Node_Pattern)
                                    return Node_Consumer'Class;

end Query;
