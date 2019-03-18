with Interpreter.Errors;         use Interpreter.Errors;
with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Primitives;     use Interpreter.Primitives;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

package body Query is

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Iter : Node_Iterator_Access;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter;

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter
   is
      Iter : constant Node_Iterator_Access :=
        new Traverse_Iterator_Wrapper'(Make_Traverse_Wrapper (Ctx.AST_Root));
   begin
      return Make_Query_Iterator (Ctx, Iter, Node);
   end Make_Query_Iterator;

   -------------------------
   -- Make_Query_iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Iter : Node_Iterator_Access;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter
   is
      Predicate : constant LEL.Expr :=
        (case Node.Kind is
            when LELCO.lkql_Filtered_Query =>
              Node.As_Filtered_Query.F_Predicate,
            when LELCO.lkql_Query =>
              No_Expr,
            when others =>
               raise Program_Error with
                 "Invalid query kind: " & Kind_Name (Node));
   begin
      return Query_Adapter (Iter, Node.F_Pattern, Ctx, Predicate);
   end Make_Query_Iterator;

   -------------------
   -- Query_Adapter --
   -------------------

   function Query_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter
   is
   begin
      return (case Pattern.Kind is
                 when LELCO.lkql_Node_Query_Pattern =>
                   Node_Query_Adapter
                     (Iter, Pattern.As_Node_Query_Pattern, Ctx, Expr),
                 when LELCO.lkql_Full_Query_Pattern =>
                   Full_Query_Adapter
                     (Iter, Pattern.As_Full_Query_Pattern, Ctx, Expr),
                 when others =>
                    raise Program_Error with
                         "Invalid query pattern kind: " & Kind_Name (Pattern));
   end Query_Adapter;

   ------------------------
   -- Node_Query_Adapter --
   ------------------------

   function Node_Query_Adapter
     (Iter          : Node_Iterator_Access;
      Query_Pattern : Node_Query_Pattern;
      Ctx           : Eval_Context_Ptr;
      Expr          : LEL.Expr) return Node_Iterators.Filter_Iter
   is
      Pattern : constant Node_Pattern := Query_Pattern.F_Queried_Node;
   begin
      return (case Pattern.Kind is
                 when LELCO.lkql_Full_Node_Pattern =>
                   Full_Node_Pattern_Adaptor
                     (Iter, Pattern.As_Full_Node_Pattern, Ctx, Expr),
                 when LELCO.lkql_Binding_Node_Pattern =>
                   Binding_Node_Pattern_Adapter
                     (Iter, Pattern.As_Binding_Node_Pattern, Ctx, Expr),
                 when LELCO.lkql_Kind_Node_Pattern =>
                   Kind_Node_Pattern_Adapter
                     (Iter, Pattern.As_Kind_Node_Pattern),
                 when others =>
                    raise Program_Error with
                      "Invalid node pattern kind: " &
                        Kind_Name (Query_Pattern));
   end Node_Query_Adapter;

   ------------------------
   -- Full_Query_Adapter --
   ------------------------

   function Full_Query_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Full_Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter
   is
      Queried_Node_Iter      : constant Node_Iterator_Access :=
        new Node_Iterators.Filter_Iter'
          (Node_Pattern_Adapter (Iter, Pattern.F_Queried_Node, Ctx, Expr));
      Related_Node_predicate : constant Node_Predicate_Access :=
        new Selector_Predicate'
          (Make_Selector_Predicate
            (Ctx, Pattern.F_Selector, Pattern.F_Related_Node));
   begin
      return Node_Iterators.Filter (Queried_Node_Iter, Related_Node_predicate);
   end Full_Query_Adapter;

   --------------------------
   -- Node_Pattern_Adapter --
   --------------------------

   function Node_Pattern_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter
   is
      Result  : Node_Iterators.Filter_Iter;
   begin
      Result := (case Pattern.Kind is
                 when LELCO.lkql_Full_Node_Pattern =>
                   Full_Node_Pattern_Adaptor
                     (Iter, Pattern.As_Full_Node_Pattern, Ctx, Expr),
                 when LELCO.lkql_Binding_Node_Pattern =>
                   Binding_Node_Pattern_Adapter
                     (Iter, Pattern.As_Binding_Node_Pattern, Ctx, Expr),
                 when LELCO.lkql_Kind_Node_Pattern =>
                   Kind_Node_Pattern_Adapter
                     (Iter, Pattern.As_Kind_Node_Pattern),
                 when others =>
                    raise Program_Error with
                      "Invalid node pattern kind: " &
                         Kind_Name (Pattern));
      return Result;
   end Node_Pattern_Adapter;

   -------------------------------
   -- Full_Node_Pattern_Adaptor --
   -------------------------------

   function Full_Node_Pattern_Adaptor
     (Iter    : Node_Iterator_Access;
      Pattern : Full_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterators.Filter_Iter
   is
      Kind_Adapter         : constant Node_Iterators.Filter_Iter :=
        Kind_Node_Pattern_Adapter (Iter, Pattern.As_Kind_Node_Pattern);
      Kind_Adapter_Ptr     : Node_Iterator_Access;
   begin
      if Expr = No_LKQL_Node then
         return Kind_Adapter;
      end if;

      Kind_Adapter_Ptr := new Node_Iterators.Filter_Iter'(Kind_Adapter);

      return Binding_Node_Pattern_Adapter
        (Kind_Adapter_Ptr, Pattern.As_Binding_Node_Pattern, Ctx, Expr);
   end Full_Node_Pattern_Adaptor;

   -------------------------------
   -- Kind_Node_Pattern_Adapter --
   -------------------------------

   function Kind_Node_Pattern_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Kind_Node_Pattern)
      return Node_Iterators.Filter_Iter
   is
      Kind : constant LALCO.Ada_Node_Kind_Type :=
        To_Ada_Node_Kind (Pattern.F_Identifier.Text);
   begin
      return Kind_Filter (Iter, Kind);
   end Kind_Node_Pattern_Adapter;

   ----------------------------------
   -- Binding_Node_Pattern_Adapter --
   ----------------------------------

   function Binding_Node_Pattern_Adapter
     (Iter    : Node_Iterator_Access;
      Pattern : Binding_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr)
      return Node_Iterators.Filter_Iter
   is
      Result    : Node_Iterators.Filter_Iter;
      Binding   : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Pattern.F_Binding.Text);
      Predicate : constant Node_Predicate_Access :=
        new Binding_Expr_Predicate'
          (Make_Binding_Expr_Predicate (Binding, Ctx, Expr));
   begin
      Result := Node_Iterators.Filter
        (Iter, Predicate);
      return Result;
   end Binding_Node_Pattern_Adapter;

   -------------------------------
   -- Selector_Pattern_Iterator --
   -------------------------------

   function Selector_Pattern_Iterator (Ctx          : Eval_Context_Ptr;
                                       Queried_Node : LAL.Ada_Node;
                                       Pattern      : Selector_Pattern)
                                       return Node_Iterator'Class
   is
   begin
      if Pattern.F_Selector_Name.Text = "children" then
         return Make_Traverse_Wrapper (Queried_Node);
      else
         Raise_Invalid_Selector_Name (Ctx, Pattern);
      end if;
   end Selector_Pattern_Iterator;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate (Self : in out Binding_Expr_Predicate;
                                 N    : LAL.Ada_Node) return Boolean
   is
      Env_Conflict : constant Boolean := Self.Ctx.Env.Contains (Self.Binding);
      Env_Backup   : constant Primitive :=
        (if Env_Conflict then Self.Ctx.Env (Self.Binding)
         else Make_Unit_Primitive);
      Expr_Result  : Primitive;
   begin
      if Self.Expr = No_Expr then
         return True;
      end if;

      Self.Ctx.Env.Include (Self.Binding, To_Primitive (N));
      Expr_Result := Typed_Eval (Self.Ctx, Self.Expr, Kind_Bool);

      if Env_Conflict then
         Self.Ctx.Env.Include (Self.Binding, Env_Backup);
      end if;

      return Bool_Val (Expr_Result);
   exception
      when Recoverable_Error =>
         if Env_Conflict then
            Self.Ctx.Env.Include (Self.Binding, Env_Backup);
         end if;
         return False;
   end Evaluate;

   -------------------------------
   -- Make_Binding_Expr_Adapter --
   -------------------------------

   function Make_Binding_Expr_Predicate (Binding : Unbounded_Text_Type;
                                         Ctx     : Eval_Context_Ptr;
                                         Expr    : LEL.Expr)
                                         return Binding_Expr_Predicate
   is
   begin
      return (Binding, Ctx, Expr);
   end Make_Binding_Expr_Predicate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (P : in out Selector_Predicate; N : LAL.Ada_Node) return Boolean
   is
      NbFiltered    : Integer;
      Selector_Iter : constant Node_Iterator_Access :=
        new Node_Iterator'Class'
          (Selector_Pattern_Iterator (P.Ctx, N, P.Selector_Pattern));
      It            : constant Node_Iterators.Filter_Iter :=
        Kind_Node_Pattern_Adapter
          (Selector_Iter,  P.Related_Node_Pattern.As_Kind_Node_Pattern);
   begin
      NbFiltered := It.Consume'Length;
      return NbFiltered > 0;
   end Evaluate;

   -----------------------------
   -- Make_Selector_Predicate --
   -----------------------------

   function Make_Selector_Predicate
     (Ctx                  : Eval_Context_Ptr;
      Selector_Pattern     : LEL.Selector_Pattern;
      Related_Node_Pattern : LEL.Node_Pattern) return Selector_Predicate
   is
   begin
      return (Ctx, Selector_Pattern, Related_Node_Pattern);
   end Make_Selector_Predicate;

   -----------------
   -- Kind_Filter --
   -----------------

   function Kind_Filter (Iter : Node_Iterator_Access;
                         Kind : LALCO.Ada_Node_Kind_Type)
                         return Node_Iterators.Filter_Iter
   is
      Kind_Predicate : constant Node_Iterators.Predicate_Access :=
        new Libadalang_Predicate_Wrapper'
          (Predicate => Libadalang.Iterators.Kind_Is (Kind));
   begin
      return Node_Iterators.Filter (Iter, Kind_Predicate);
   end Kind_Filter;

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Traverse_Iterator_Wrapper;
                             Result : out LAL.Ada_Node) return Boolean
   is
   begin
      return Iter.Inner.Next (Result);
   end Next;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate (P : in out Libadalang_Predicate_Wrapper;
                                 N : LAL.Ada_Node) return Boolean
   is
   begin
      return P.Predicate.Get.Evaluate (N);
   end Evaluate;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Traverse_Iterator_Wrapper) is
   begin
      Free_Traverse_Iterator (Iter.Inner);
   end Release;

   ---------------------------
   -- Make_Traverse_Wrapper --
   ---------------------------

   function Make_Traverse_Wrapper
     (Root : LAL.Ada_Node) return Traverse_Iterator_Wrapper
   is
      Inner_Iter : constant Ada_Node_Iterator_Access :=
        new LAL_Node_Iterator'
          (LAL_Node_Iterator (Libadalang.Iterators.Traverse (Root)));
   begin
      return (Inner => Inner_Iter);
   end Make_Traverse_Wrapper;

end Query;
