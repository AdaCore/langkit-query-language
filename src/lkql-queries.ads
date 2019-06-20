with LKQL.AST_Nodes;       use LKQL.AST_Nodes;
with LKQL.Depth_Nodes;     use LKQL.Depth_Nodes;
with LKQL.Eval_Contexts;   use LKQL.Eval_Contexts;
with LKQL.Chained_Pattern; use LKQL.Chained_Pattern;

private package LKQL.Queries is

   function Make_Query_Iterator (Ctx  : Eval_Context;
                                 Node : L.Query)
                                 return AST_Node_Iterator'Class;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  belong to the result of the given query.

   function Make_Query_Iterator (Ctx     : Eval_Context;
                                 Pattern : L.Base_Pattern)
                                 return AST_Node_Iterator'Class;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  match the given pattern.

private

   function Make_Chained_Pattern_Query_Iterator
     (Ctx : Eval_Context;
      Node : L.Query) return AST_Node_Iterator'Class;

   ---------------------
   -- Query_Predicate --
   ---------------------

   type Query_Predicate is new AST_Node_Iterator_Predicate with record
      Ctx     : Eval_Context;
      Pattern : L.Base_Pattern;
   end record;
   --  Predicate that returns true for every node that belongs to the
   --  result of the given query.

   type Query_Predicate_Access is access all Query_Predicate;
   --  Pointer to a Query_predicate

   function Make_Query_Predicate
     (Ctx : Eval_Context; Pattern : L.Base_Pattern)
      return Query_Predicate_Access;
   --  Return a pointer to a Query_Predicate that returns true for every node
   --  that belongs to the result set of the given query.

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : AST_Node_Rc) return Boolean;
   --  Evaluate the given predicate against 'Node'

   overriding function Clone
     (Self : Query_Predicate) return Query_Predicate;
   --  Return a copy of the given Query_Predicate

   overriding procedure Release (Self : in out Query_Predicate);

   --------------------------------
   -- Chained_Pattern_Query_Iter --
   --------------------------------

   type Chained_Pattern_Query_Iter is new AST_Node_Iterator with record
      Ctx       : Eval_Context;
      Predicate : L.Expr;
      Iter      : Chained_Pattern_Iterator;
   end record;

   overriding function Next (Iter   : in out Chained_Pattern_Query_Iter;
                             Result : out AST_Node_Rc) return Boolean;

   overriding function Clone (Iter : Chained_Pattern_Query_Iter)
                              return Chained_Pattern_Query_Iter;

   overriding procedure Release (Iter : in out Chained_Pattern_Query_Iter);

end LKQL.Queries;
