with Interpreter.Primitives;     use Interpreter.Primitives;
with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Liblkqllang.Common; use type Liblkqllang.Common.LKQL_Node_Kind_Type;

with Langkit_Support.Text; use Langkit_Support.Text;

package body Query.Patterns is

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter
   is
      Iter      : constant Node_Iterator_Access :=
        new Childs_Iterator'(Make_Childs_Iterator (Ctx.AST_Root));
      Predicate : constant Iterator_Predicate_Access :=
        Iterator_Predicate_Access (Make_Query_Predicate (Ctx, Node));
   begin
      return Node_Iterators.Filter (Iter, Predicate);
   end Make_Query_Iterator;

   --------------------------
   -- Make_Query_Predicate --
   --------------------------

   function Make_Query_Predicate
     (Ctx : Eval_Context_Ptr; Query : LEL.Query) return Query_Predicate_Access
   is
   begin
      return new Query_Predicate'(Ctx, Query);
   end Make_Query_Predicate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : Iterator_Node) return Boolean
   is
   begin
      return (case Self.Query.Kind is
                 when LELCO.LKQL_Query =>
                   Match_Unfiltered_Query (Self.Ctx, Self.Query, Node),
                 when LELCO.LKQL_Filtered_Query =>
                   Match_Filtered_Query
                      (Self.Ctx, Self.Query.As_Filtered_Query, Node),
                 when others =>
                    raise Program_Error with
                      "Invalid query kind: " & LEL.Kind_Name (Self.Query));
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

   -----------------
   -- Match_Query --
   -----------------

   function Match_Unfiltered_Query (Ctx   : Eval_Context_Ptr;
                                    Query : LEL.Query;
                                    Node  : Iterator_Node) return Boolean
   is
   begin
      return Match_Query_Pattern (Ctx, Query.F_Pattern, Node).Success;
   end Match_Unfiltered_Query;

   --------------------------
   -- Match_Filtered_Query --
   --------------------------

   function Match_Filtered_Query (Ctx   : Eval_Context_Ptr;
                                  Query : LEL.Filtered_Query;
                                  Node  : Iterator_Node) return Boolean
   is
      Query_Match : constant Match :=
        Match_Query_Pattern (Ctx, Query.F_Pattern, Node);
   begin
      if not Query_Match.Success then
         return False;
      end if;

      return
        Bool_Val
          (Eval (Ctx, Query.F_Predicate, Kind_Bool, Query_Match.Bindings));
   end Match_Filtered_Query;

   -------------------------
   -- Match_Query_Pattern --
   -------------------------

   function Match_Query_Pattern (Ctx           : Eval_Context_Ptr;
                                 Query_Pattern : LEL.Query_Pattern;
                                 Node          : Iterator_Node)
                                 return Match
   is
   begin
      return (case Query_Pattern.Kind is
                 when LELCO.LKQL_Node_Query_Pattern =>
                   Match_Node_Pattern
                     (Query_Pattern.As_Node_Query_Pattern.F_Queried_Node,
                      Node.Node),
                 when LELCO.LKQL_Full_Query_Pattern =>
                   Match_Full_Query_Pattern
                     (Ctx, Query_Pattern.As_Full_Query_Pattern, Node),
                 when others =>
                    raise Program_Error with
                      "Invalid query pattern kind: " &
                      LEL.Kind_Name (Query_Pattern));
   end Match_Query_Pattern;

   ------------------------------
   -- Match_Full_Query_Pattern --
   ------------------------------

   function Match_Full_Query_Pattern
     (Ctx           : Eval_Context_Ptr;
      Query_Pattern : LEL.Full_Query_Pattern;
      Node          : Iterator_Node) return Match
   is
      use String_Value_Maps;
      Queried_Match          : constant Match :=
        Match_Node_Pattern (Query_Pattern.F_Queried_Node, Node.Node);
      Related_Nodes_Iter     : Node_Iterator'Class :=
        Make_Selector_Iterator
          (Ctx, Node, Query_Pattern.F_Selector);
      Related_Nodes_Consumer : Node_Consumer'Class :=
        Make_Selector_Consumer
          (Query_Pattern.F_Selector, Query_Pattern.F_Related_Node);
      Selector_Match         : Match;
      Bindings               : Map;
   begin
      if not Queried_Match.Success then
         Related_Nodes_Iter.Release;
         return Match_Failure;
      end if;

      Update_Env (Bindings, Queried_Match.Bindings);
      Selector_Match := Related_Nodes_Consumer.Consume (Related_Nodes_Iter);

      if not Selector_Match.Success then
         return Match_Failure;
      end if;

      Update_Env (Bindings, Selector_Match.Bindings);

      return (Success => True, Bindings => Bindings);
   end Match_Full_Query_Pattern;

   ------------------------
   -- Match_Node_Pattern --
   ------------------------

   function Match_Node_Pattern (Node_Pattern : LEL.Node_Pattern;
                                Node         : LAL.Ada_Node)
                                return Match
   is
   begin
      return (case Node_Pattern.Kind is
                 when LELCO.LKQL_Kind_Node_Pattern =>
                   Match_Kind_Node_Pattern
                     (Node_Pattern.As_Kind_Node_Pattern, Node),
                 when LELCO.LKQL_Binding_Node_Pattern =>
                   Match_Binding_Node_Pattern
                     (Node_Pattern.As_Binding_Node_Pattern, Node),
                 when LELCO.LKQL_Full_Node_Pattern =>
                   Match_Full_Node_Pattern
                     (Node_Pattern.As_Full_Node_Pattern, Node),
                 when others =>
                    raise Program_Error with
                      "invalid node pattern kind: " &
                      LEL.Kind_Name (Node_Pattern));
   end Match_Node_Pattern;

   --------------------------------
   -- Match_Binding_Node_Pattern --
   --------------------------------

   function Match_Binding_Node_Pattern
     (Node_Pattern : LEL.Binding_Node_Pattern;
      Node         : LAL.Ada_Node) return Match
   is
      use String_Value_Maps;
      Bindings : Map;
      Name     : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node_Pattern.P_Binding_Name);
   begin
      Bindings.Insert (Name, To_Primitive (Node));
      return (Success => True, Bindings => Bindings);
   end Match_Binding_Node_Pattern;

   -----------------------------
   -- Match_Kind_Node_Pattern --
   -----------------------------

   function Match_Kind_Node_Pattern
     (Node_Pattern : LEL.Kind_Node_Pattern;
      Node         : LAL.Ada_Node)
      return Match
   is
      use type LALCO.Ada_Node_Kind_Type;
      Expected_Kind : constant LALCO.Ada_Node_Kind_Type :=
        To_Ada_Node_Kind (Node_Pattern.F_Identifier.Text);
      Success       : constant Boolean := Node.Kind = Expected_Kind;
   begin
      return (Success, String_Value_Maps.Empty_Map);
   end Match_Kind_Node_Pattern;

   -----------------------------
   -- Match_Full_Node_Pattern --
   -----------------------------

   function Match_Full_Node_Pattern
     (Node_Pattern : LEL.Full_Node_Pattern; Node : LAL.Ada_Node) return Match
   is
      Binding_Match : constant Match :=
        Match_Binding_Node_Pattern
          (Node_Pattern.F_Binding_Pattern, Node);
      Kind_Match    : constant Match :=
        Match_Kind_Node_Pattern
          (Node_Pattern.F_Kind_Pattern, Node);
   begin
      return Binding_Match'Update (Success => Kind_Match.Success);
   end Match_Full_Node_Pattern;

   ----------------------------
   -- Make_Selector_Iterator --
   ----------------------------

   function Make_Selector_Iterator
     (Ctx              : Eval_Context_Ptr;
      Queried_Node     : Iterator_Node;
      Selector_Pattern : LEL.Selector_Pattern'Class)
      return Node_Iterator'Class
   is
      Base_Selector : constant Node_Iterator'Class :=
        Selector_Iterator_From_Name (Ctx, Queried_Node.Node, Selector_Pattern);
   begin
      if Selector_Pattern.P_Condition.Is_Null then
         return Base_Selector;
      end if;

      return
        Node_Iterators.Filter
         (Base_Selector,
          Selector_Conditions_Predicate'(Ctx, Selector_Pattern.P_Condition));
   end Make_Selector_Iterator;

   ---------------------------------
   -- Selector_Iterator_From_Name --
   ---------------------------------

   function Selector_Iterator_From_Name
     (Ctx           : Eval_Context_Ptr;
      Queried_Node  : LAL.Ada_Node;
      Selector_Pattern : LEL.Selector_Pattern'Class)
      return Node_Iterator'Class
   is
   begin
      if Selector_Pattern.P_Selector_Name = "children" then
         return Make_Childs_Iterator (Queried_Node);
      else
         Raise_Invalid_Selector_Name (Ctx, Selector_Pattern);
      end if;
   end Selector_Iterator_From_Name;

   ----------------------------
   -- Make_Selector_Consumer --
   ----------------------------

   function Make_Selector_Consumer (Selector     : LEL.Selector_Pattern;
                                    Related_Node : LEL.Node_Pattern)
                                    return Node_Consumer'Class
   is
      Quantifier_Name : constant Text_Type :=
        Selector.P_Quantifier_Name;
   begin
      return (if Quantifier_Name = "some" then
                 Exists_Consumer'(Pattern => Related_Node)
              elsif Quantifier_Name = "all" then
                 All_Consumer'(Pattern => Related_Node)
              else raise Program_Error);
   end Make_Selector_Consumer;

   -------------
   -- Consume --
   -------------

   function Consume (Self : in out Exists_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match
   is
      use String_Value_Maps;
      Bindings      : Map;
      Current_Node  : Iterator_Node;
      Current_Match : Match;
      Matched       : Boolean := False;
      Nodes         : constant Primitive := Make_Empty_List;
      Save_Bindings : constant Boolean := Self.Pattern.P_Has_Binding;
   begin
      while Iter.Next (Current_Node) loop
         Current_Match := Match_Node_Pattern (Self.Pattern, Current_Node.Node);

         if Current_Match.Success then
            Matched := True;

            if Save_Bindings then
               Append (Nodes, To_Primitive (Current_Node.Node));
            end if;
         end if;
      end loop;

      Iter.Release;

      if not Matched then
         return Match_Failure;
      end if;

      if Save_Bindings then
         Bindings.Insert
           (To_Unbounded_Text (Self.Pattern.P_Binding_Name), Nodes);
      end if;

      return (Success => True, Bindings => Bindings);
   end Consume;

   -------------
   -- Consume --
   -------------

   function Consume (Self : in out All_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match
   is
      use String_Value_Maps;
      Current_Node  : Iterator_Node;
      Current_Match : Match;
      Bindings      : Map;
      Nodes         : constant Primitive := Make_Empty_List;
      Save_Bindings : constant Boolean := Self.Pattern.P_Has_Binding;
   begin
      while Iter.Next (Current_Node) loop
         Current_Match := Match_Node_Pattern (Self.Pattern, Current_Node.Node);

         if not Current_Match.Success then
            Iter.Release;
            return Match_Failure;
         elsif Save_Bindings then
            Append (Nodes, To_Primitive (Current_Node.Node));
         end if;
      end loop;

      if Save_Bindings then
         Bindings.Insert
           (To_Unbounded_Text (Self.Pattern.P_Binding_Name), Nodes);
      end if;

      return (True, Bindings);
   end Consume;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Self    : in out Selector_Conditions_Predicate;
      Element : Iterator_Node)
      return Boolean
   is
      Local_Env   : Environment;
      Eval_Result : Primitive;
   begin
      Local_Env.Insert
        (To_Unbounded_Text ("depth"), To_Primitive (Element.Depth));
      Eval_Result :=
        Eval (Self.Context, Self.Condition, Kind_Bool, Local_Env);
      return Bool_Val (Eval_Result);
   end Evaluate;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Self : Selector_Conditions_Predicate)
                              return Selector_Conditions_Predicate
   is
   begin
      return Selector_Conditions_Predicate'(Self.Context, Self.Condition);
   end Clone;

end Query.Patterns;
