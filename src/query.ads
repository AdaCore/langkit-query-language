with Iterators;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Common;
with Liblkqllang.Analysis; use Liblkqllang.Analysis;

with Libadalang.Common;
with Libadalang.Analysis;
with Libadalang.Iterators;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Unchecked_Deallocation;

package Query is
   package LEL renames Liblkqllang.Analysis;
   package LELCO renames Liblkqllang.Common;
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;

   package Node_Iterators is new Iterators (LAL.Ada_Node);
   --  Iterator over a sequence of Ada_Node values

   subtype Node_Iterator_Access is Node_Iterators.Iterator_Access;
   --  Raw pointer to an iterator over a sequence of Ada_Node values

   subtype Node_Iterator is Node_Iterators.Iterator;
   --  Interface implemented by iterators over Ada_Node values

   subtype Node_Predicate_Interface is Node_Iterators.Predicates.Func;
   --  Interface implemented by predicates that take an Ada_Node argument

   subtype Node_Predicate_Access is Node_Iterators.Predicate_Access;

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  belong to the result of the given query.

   function Query_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter;
   --  Returns an iterator that wraps Iter, yielding only the elements that
   --  belong to the result of the given query.

   function Node_Query_Adapter
     (Iter          : Node_Iterator_Access;
      Query_Pattern : Node_Query_Pattern;
      Ctx           : Eval_Context_Ptr;
      Expr          : LEL.Expr) return Node_Iterators.Filter_Iter;
   --  Given a query pattern comprised of a single node pattern, return an
   --  iterator tha wraps Iter, yielding only the elements that match the
   --  pattern.

   function Full_Query_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Full_Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter;
   --  Given a query pattern comprised of a node pattern, a selector and a
   --  related node ppatern, returns an iterator that wraps Iter, yielding only
   --  the elements that match the given pattern and for witch the related node
   --  matched the related node pattern.

   function Node_Pattern_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter;

   function Full_Node_Pattern_Adaptor
     (Iter    : Node_Iterator_Access;
      Pattern : Full_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter;
   --  Given a node pattern comprised of a node kind pattern and a binding
   --  name, return an iterator that wraps Iter, yielding only the elements of
   --  the given kind for witch the predicate 'Expr' is verrified when the
   --  binding name is bound to the value of the element.

   function Kind_Node_Pattern_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Kind_Node_Pattern)
      return Node_Iterators.Filter_Iter;
   --  Return an iterator that wraps Iter, yielding only the elements that
   --  match the Ada node kind described in 'Pattern'.

   function Binding_Node_Pattern_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Binding_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr)
      return Node_Iterators.Filter_Iter;
   --  Return an iterator that wraps Iter, yielding only the elements x for
   --  which the evaluation of Expr in a context in wich x is associated with
   --  the name specified in Pattern returns True.

   function Selector_Pattern_Iterator (Ctx          : Eval_Context_Ptr;
                                       Queried_Node : LAL.Ada_Node;
                                       Pattern      : Selector_Pattern)
                                       return Node_Iterators.Iterator'Class;

private

   type Binding_Expr_Predicate is new Node_Predicate_Interface with record
      Binding : Unbounded_Text_Type;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr;
   end record;

   overriding function Evaluate
     (Self : in out Binding_Expr_Predicate; N : LAL.Ada_Node) return Boolean;

   function Make_Binding_Expr_Predicate
     (Binding : Unbounded_Text_Type; Ctx : Eval_Context_Ptr; Expr : LEL.Expr)
      return Binding_Expr_Predicate;
   --  Return a ref-counted pointer pointing to a Binding_Expr_Predicate with
   --  the given Binding, Ctx and Expr.

   type Selector_Predicate is new Node_Predicate_Interface with record
      Ctx                  : Eval_Context_Ptr;
      Selector_Pattern     : LEL.Selector_Pattern;
      Related_Node_Pattern : LEL.Node_Pattern;
   end record;

   overriding function Evaluate
     (P : in out Selector_Predicate; N : LAL.Ada_Node) return Boolean;

   function Make_Selector_Predicate
     (Ctx                  : Eval_Context_Ptr;
      Selector_Pattern     : LEL.Selector_Pattern;
      Related_Node_Pattern : LEL.Node_Pattern) return Selector_Predicate;

   type Libadalang_Predicate_Wrapper is new Node_Predicate_Interface
   with record
      Predicate : Libadalang.Iterators.Ada_Node_Predicate;
   end record;

   overriding function Evaluate (P : in out Libadalang_Predicate_Wrapper;
                                 N : LAL.Ada_Node) return Boolean;

   function Kind_Filter (Iter : Node_Iterator_Access;
                         Kind : LALCO.Ada_Node_Kind_Type)
                         return Node_Iterators.Filter_Iter;
   --  Return an iterator over the elements of 'Iter' that filters the element
   --  which kind is different than 'Kind'.

   subtype LAL_Node_Iterator is
     Libadalang.Iterators.Ada_Node_Iterators.Iterator'Class;

   type Ada_Node_Iterator_Access is
     access all LAL_Node_Iterator;

   procedure Free_Traverse_Iterator is new Ada.Unchecked_Deallocation
     (LAL_Node_Iterator,
      Ada_Node_Iterator_Access);

   type Traverse_Iterator_Wrapper is new Node_Iterator with record
      Inner : Ada_Node_Iterator_Access;
   end record;

   overriding function Next (Iter   : in out Traverse_Iterator_Wrapper;
                             Result : out LAL.Ada_Node) return Boolean;

   overriding procedure Release (Iter : in out Traverse_Iterator_Wrapper);

   function Make_Traverse_Wrapper
     (Root : LAL.Ada_Node) return Traverse_Iterator_Wrapper;

end Query;
