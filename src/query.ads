with Iterators;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Common;
with Liblkqllang.Analysis; use Liblkqllang.Analysis;

with Libadalang.Common;
with Libadalang.Analysis;
with Libadalang.Iterators; use Libadalang.Iterators;

with Langkit_Support.Text; use Langkit_Support.Text;

package Query is
   package LEL renames Liblkqllang.Analysis;
   package LELCO renames Liblkqllang.Common;
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   subtype Query_Iterator is Ada_Node_Iterators.Iterator'Class;
   --  Iterator over Ada nodes

   type Query_Iterator_Access is
     access all Query_Iterator;

   package Node_Iterator_Filter is new Iterators.Filters
     (Query_Iterator);
   --  Iterators that performs filtering on top of a Query_Iterator

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterator_Filter.Filter_Iter;

   function Query_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterator_Filter.Filter_Iter;
   --  Returns an iterator that wraps Iter, yielding only the elements that
   --  belong to the result of the given query.

   function Node_Query_Adapter
     (Iter          : Query_Iterator_Access;
      Query_Pattern : Node_Query_Pattern;
      Ctx           : Eval_Context_Ptr;
      Expr          : LEL.Expr) return Node_Iterator_Filter.Filter_Iter;
   --  Given a query pattern comprised of a single node pattern, return an
   --  iterator tha wraps Iter, yielding only the elements that match the
   --  pattern.

   function Full_Query_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Full_Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterator_Filter.Filter_Iter;
   --  Given a query pattern comprised of a node pattern, a selector and a
   --  related node ppatern, returns an iterator that wraps Iter, yielding only
   --  the elements that match the given pattern and for witch the related node
   --  matched the related node pattern.

   function Full_Node_Pattern_Adaptor
     (Iter    : Query_Iterator_Access;
      Pattern : Full_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterator_Filter.Filter_Iter;
   --  Given a node pattern comprised of a node kind pattern and a binding
   --  name, return an iterator that wraps Iter, yielding only the elements of
   --  the given kind for witch the predicate 'Expr' is verrified when the
   --  binding name is bound to the value of the element.

   function Kind_Node_Pattern_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Kind_Node_Pattern)
      return Node_Iterator_Filter.Filter_Iter;
   --  Return an iterator that wraps Iter, yielding only the elements that
   --  match the Ada node kind described in 'Pattern'.

   function Binding_Node_Pattern_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Binding_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr)
      return Node_Iterator_Filter.Filter_Iter;
   --  Return an iterator that wraps Iter, yielding only the elements x for
   --  which the evaluation of Expr in a context in wich x is associated with
   --  the name specified in Pattern returns True.

private

   type Binding_Expr_Predicate is new Ada_Node_Predicate_Interface with record
      Binding : Unbounded_Text_Type;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr;
   end record;

   overriding function Evaluate
     (P : in out Binding_Expr_Predicate; N : LAL.Ada_Node) return Boolean;

   function Make_Binding_Expr_Predicate
     (Binding : Unbounded_Text_Type; Ctx : Eval_Context_Ptr; Expr : LEL.Expr)
      return Ada_Node_Predicate;
   --  Return a ref-counted pointer pointing to a Binding_Expr_Predicate with
   --  the given Binding, Ctx and Expr.

   function Kind_Filter
     (Iter : Query_Iterator_Access; Kind : LALCO.Ada_Node_Kind_Type)
      return Node_Iterator_Filter.Filter_Iter;
   --  Return an iterator over the elements of 'Iter' that filters the element
   --  which kind is different than 'Kind'.

end Query;
