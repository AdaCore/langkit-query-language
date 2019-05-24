with LKQL.Patterns;       use LKQL.Patterns;
with LKQL.Patterns.Nodes; use LKQL.Patterns.Nodes;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with LKQL.Primitives;     use LKQL.Primitives;

package body LKQL.Queries is

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context;
                                 Node : L.Query)
                                 return Depth_Node_Iters.Filter_Iter
   is
      Iter      : constant Depth_Node_Iter_Access :=
        new Childs_Iterator'(Make_Childs_Iterator (Ctx.AST_Root));
      Predicate : constant Iterator_Predicate_Access :=
        Iterator_Predicate_Access (Make_Query_Predicate (Ctx, Node));
   begin
      return Depth_Node_Iters.Filter (Iter, Predicate);
   end Make_Query_Iterator;

   --------------------------
   -- Make_Query_Predicate --
   --------------------------

   function Make_Query_Predicate
     (Ctx : Eval_Context; Query : L.Query) return Query_Predicate_Access
   is
   begin
      return new Query_Predicate'(Ctx, Query);
   end Make_Query_Predicate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : Depth_Node) return Boolean
   is
      Match : constant Match_Result :=
        Match_Pattern (Self.Ctx,
                    Self.Query.F_Pattern,
                    To_Primitive (Node.Node));
   begin
      return Match.Is_Success;
   end Evaluate;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Self : Query_Predicate) return Query_Predicate
   is
   begin
      return Query_Predicate'(Self.Ctx, Self.Query);
   end Clone;

end LKQL.Queries;
