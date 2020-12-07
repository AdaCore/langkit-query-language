with LKQL.Patterns;       use LKQL.Patterns;
with LKQL.Primitives;     use LKQL.Primitives;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_AST_Nodes; use Ada_AST_Nodes;

package body LKQL.Queries is

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context;
                                 Node : L.Query)
                                 return AST_Node_Iterator'Class
   is
   begin
      if Node.F_Pattern.P_Contains_Chained then
         declare
            Chained : constant Chained_Pattern_Iterator :=
              Make_Chained_Pattern_Iterator
                (Ctx, Node.F_Pattern.P_Value_Part.As_Chained_Node_Pattern);
         begin
            return Chained_Pattern_Query_Iter'
              (Ctx       => Ctx.Clone_Frame,
               Iter      => Chained);
         end;
      else
         declare
            Iter      : constant AST_Node_Iterator_Access :=
              new AST_Node_Iterator'Class'
                (AST_Node_Iterator'Class
                   (Make_Child_Iterator (Ctx.AST_Roots.all)));

            Predicate : constant AST_Node_Predicate_Access :=
              AST_Node_Predicate_Access
                (Make_Query_Predicate (Ctx, Node.F_Pattern));
         begin
            return AST_Node_Iterators.Filter (Iter, Predicate);
         end;
      end if;
   end Make_Query_Iterator;

   --------------------------
   -- Make_Query_Predicate --
   --------------------------

   function Make_Query_Predicate
     (Ctx : Eval_Context; Pattern : L.Base_Pattern)
      return Query_Predicate_Access
   is
   begin
      return new Query_Predicate'(Ctx.Clone_Frame, Pattern);
   end Make_Query_Predicate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : AST_Node_Rc) return Boolean
   is
      Match : constant Match_Result :=
        Match_Pattern (Self.Ctx,
                       Self.Pattern,
                       To_Primitive (Node));
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
      return Query_Predicate'(Self.Ctx, Self.Pattern);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Query_Predicate) is
   begin
      Self.Ctx.Release_Current_Frame;
   end Release;

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Chained_Pattern_Query_Iter;
                             Result : out AST_Node_Rc) return Boolean
   is
      Match            : Match_Result;
   begin
      --  The inner iterator is empty: return false
      if not Iter.Iter.Next (Match) then
         return False;
      end if;

      Result := Node_Val (Match.Get_Matched_Value);
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Chained_Pattern_Query_Iter)
                              return Chained_Pattern_Query_Iter
   is
     (Iter.Ctx.Clone_Frame, Iter.Iter.Clone);

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Chained_Pattern_Query_Iter) is
   begin
      Iter.Ctx.Release_Current_Frame;
      Iter.Iter.Release;
   end Release;

end LKQL.Queries;
