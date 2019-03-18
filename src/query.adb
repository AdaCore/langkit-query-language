with Interpreter.Errors;     use Interpreter.Errors;
with Interpreter.Evaluation; use Interpreter.Evaluation;
with Interpreter.Primitives; use Interpreter.Primitives;

package body Query is

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterator_Filter.Filter_Iter
   is
      It        : constant Query_Iterator_Access :=
        new Query_Iterator'(Query_Iterator (Traverse (Ctx.AST_Root)));
      Predicate : constant LEL.Expr :=
        (case Node.Kind is
            when LELCO.lkql_Filtered_Query =>
              Node.As_Filtered_Query.F_Predicate,
            when LELCO.lkql_Query =>
              No_Expr,
            when others =>
               raise Program_Error with
                 "Invalid query kind: " & Kind_Name (Node));
      Result : constant Node_Iterator_Filter.Filter_Iter :=
        Query_Adapter (It, Node.F_Pattern, Ctx, Predicate);
   begin
      return Result;
   end Make_Query_Iterator;

   -------------------
   -- Query_Adapter --
   -------------------

   function Query_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterator_Filter.Filter_Iter
   is
      Result : Node_Iterator_Filter.Filter_Iter;
   begin
      Result := (case Pattern.Kind is
                 when LELCO.lkql_Node_Query_Pattern =>
                   Node_Query_Adapter
                     (Iter, Pattern.As_Node_Query_Pattern, Ctx, Expr),
                 when LELCO.lkql_Full_Query_Pattern =>
                   Full_Query_Adapter
                     (Iter, Pattern.As_Full_Query_Pattern, Ctx, Expr),
                 when others =>
                    raise Program_Error with
                         "Invalid query pattern kind: " & Kind_Name (Pattern));
      return Result;
   end Query_Adapter;

   ------------------------
   -- Node_Query_Adapter --
   ------------------------

   function Node_Query_Adapter
     (Iter          : Query_Iterator_Access;
      Query_Pattern : Node_Query_Pattern;
      Ctx           : Eval_Context_Ptr;
      Expr          : LEL.Expr) return Node_Iterator_Filter.Filter_Iter
   is
      Pattern : constant Node_Pattern := Query_Pattern.F_Queried_Node;
      Result  : Node_Iterator_Filter.Filter_Iter;
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
                        Kind_Name (Query_Pattern));
      return Result;
   end Node_Query_Adapter;

   ------------------------
   -- Full_Query_Adapter --
   ------------------------

   function Full_Query_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Full_Query_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterator_Filter.Filter_Iter
   is
      pragma Unreferenced (Iter, Pattern, Ctx, Expr);
      Result : Node_Iterator_Filter.Filter_Iter;
   begin
      raise Program_Error with "Not implemented (yet) !";
      return Result;
   end Full_Query_Adapter;

   -------------------------------
   -- Full_Node_Pattern_Adaptor --
   -------------------------------

   function Full_Node_Pattern_Adaptor
     (Iter    : Query_Iterator_Access;
      Pattern : Full_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr) return Node_Iterator_Filter.Filter_Iter
   is
      Kind_Adapter         : constant Node_Iterator_Filter.Filter_Iter :=
        Kind_Node_Pattern_Adapter (Iter, Pattern.As_Kind_Node_Pattern);
      Kind_Adapter_Ptr     : Query_Iterator_Access;
   begin
      if Expr = No_LKQL_Node then
         return Kind_Adapter;
      end if;

      Kind_Adapter_Ptr := new Node_Iterator_Filter.Filter_Iter'(Kind_Adapter);

      return Binding_Node_Pattern_Adapter
        (Kind_Adapter_Ptr, Pattern.As_Binding_Node_Pattern, Ctx, Expr);
   end Full_Node_Pattern_Adaptor;

   -------------------------------
   -- Kind_Node_Pattern_Adapter --
   -------------------------------

   function Kind_Node_Pattern_Adapter
     (Iter    : Query_Iterator_Access;
      Pattern : Kind_Node_Pattern)
      return Node_Iterator_Filter.Filter_Iter
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
     (Iter    : Query_Iterator_Access;
      Pattern : Binding_Node_Pattern;
      Ctx     : Eval_Context_Ptr;
      Expr    : LEL.Expr)
      return Node_Iterator_Filter.Filter_Iter
   is
      Result    : Node_Iterator_Filter.Filter_Iter;
      Binding   : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Pattern.F_Binding.Text);
      Predicate : constant Ada_Node_Predicate :=
        Make_Binding_Expr_Predicate (Binding, Ctx, Expr);
   begin
      Result := Node_Iterator_Filter.Filter
        (Node_Iterator_Filter.Iter_Type_Access (Iter), Predicate);
      return Result;
   end Binding_Node_Pattern_Adapter;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate (P : in out Binding_Expr_Predicate;
                                 N : LAL.Ada_Node) return Boolean
   is
      Env_Conflict : constant Boolean := P.Ctx.Env.Contains (P.Binding);
      Env_Backup   : constant Primitive :=
        (if Env_Conflict then P.Ctx.Env (P.Binding) else Make_Unit_Primitive);
      Expr_Result  : Primitive;
   begin
      P.Ctx.Env.Include (P.Binding, To_Primitive (N));
      Expr_Result := Typed_Eval (P.Ctx, P.Expr, Kind_Bool);

      if Env_Conflict then
         P.Ctx.Env.Include (P.Binding, Env_Backup);
      end if;

      return Bool_Val (Expr_Result);
   exception
      when Recoverable_Error =>
         if Env_Conflict then
            P.Ctx.Env.Include (P.Binding, Env_Backup);
         end if;
         return False;
   end Evaluate;

   -------------------------------
   -- Make_Binding_Expr_Adapter --
   -------------------------------

   function Make_Binding_Expr_Predicate (Binding : Unbounded_Text_Type;
                                         Ctx     : Eval_Context_Ptr;
                                         Expr    : LEL.Expr)
                                         return Ada_Node_Predicate
   is
      Adapter : constant Binding_Expr_Predicate := (Binding, Ctx, Expr);
   begin
      return Result : Ada_Node_Predicate do
         Result.Set (Adapter);
      end return;
   end Make_Binding_Expr_Predicate;

   -----------------
   -- Kind_Filter --
   -----------------

   function Kind_Filter (Iter : Query_Iterator_Access;
                         Kind : LALCO.Ada_Node_Kind_Type)
                         return Node_Iterator_Filter.Filter_Iter
   is
   begin
      return Node_Iterator_Filter.Filter
        (Node_Iterator_Filter.Iter_Type_Access (Iter), Kind_Is (Kind));
   end Kind_Filter;

end Query;
