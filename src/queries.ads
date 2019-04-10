with Patterns.Nodes;            use Patterns.Nodes;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Analysis;
with Liblkqllang.Common;

with Libadalang.Analysis;
with Libadalang.Common;

package Queries is

   package L renames Liblkqllang.Analysis;
   package LCO renames Liblkqllang.Common;

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   subtype Iterator_Predicate_Interface is Node_Iterators.Predicates.Func;
   --  Predicates on 'Iterator_Node' values

   subtype Iterator_Predicate_Access is Node_Iterators.Predicate_Access;
   --  Pointer to a predicate on 'Iterator_Node' values

   function Make_Query_Iterator (Ctx  : Eval_Context;
                                 Node : L.Query)
                                 return Node_Iterators.Filter_Iter;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  belong to the result of the given query.

private

   ---------------------
   -- Query_Predicate --
   ---------------------

   type Query_Predicate is new Iterator_Predicate_Interface with record
      Ctx   : Eval_Context;
      Query : L.Query;
   end record;
   --  Predicate that returns true for every node that belongs to the
   --  result of the given query.

   type Query_Predicate_Access is access all Query_Predicate;
   --  Pointer to a Query_predicate

   function Make_Query_Predicate
     (Ctx : Eval_Context; Query : L.Query) return Query_Predicate_Access;
   --  Return a pointer to a Query_Predicate that returns true for every node
   --  that belongs to the result set of the given query.

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : Iterator_Node) return Boolean;
   --  Evaluae the given predicate against 'Node'

   overriding function Clone
     (Self : Query_Predicate) return Query_Predicate;
   --  Return a copy of the given Query_Predicate

end Queries;
