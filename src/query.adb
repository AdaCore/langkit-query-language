with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Primitives;     use Interpreter.Primitives;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Liblkqllang.Common; use type Liblkqllang.Common.LKQL_Node_Kind_Type;

package body Query is

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context_Ptr;
                                 Node : LEL.Query)
                                 return Node_Iterators.Filter_Iter
   is
      Iter      : constant Node_Iterator_Access :=
        new Traverse_Wrapper'(Make_Travers_Wrapper (Ctx.AST_Root));
      Predicate : constant Node_Iterators.Predicate_Access :=
        Node_Iterators.Predicate_Access (Make_Query_Predicate (Ctx, Node));
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
     (Self : in out Query_Predicate; Node : LAL.Ada_Node) return Boolean
   is
   begin
      return (case Self.Query.Kind is
                 when LELCO.lkql_Query =>
                   Match_Unfiltered_Query (Self.Ctx, Self.Query, Node),
                 when LELCO.lkql_Filtered_Query =>
                   Match_Filtered_Query
                      (Self.Ctx, Self.Query.As_Filtered_Query, Node),
                 when others =>
                    raise Program_Error with
                      "Invalid query kind: " & LEL.Kind_Name (Self.Query));
   end Evaluate;

   -----------------
   -- Match_Query --
   -----------------

   function Match_Unfiltered_Query (Ctx   : Eval_Context_Ptr;
                                    Query : LEL.Query;
                                    Node  : LAL.Ada_Node) return Boolean
   is
   begin
      return Match_Query_Pattern (Ctx, Query.F_Pattern, Node).Success;
   end Match_Unfiltered_Query;

   --------------------------
   -- Match_Filtered_Query --
   --------------------------

   function Match_Filtered_Query (Ctx   : Eval_Context_Ptr;
                                  Query : LEL.Filtered_Query;
                                  Node  : LAL.Ada_Node) return Boolean
   is
      use String_Value_Maps;
      Result      : Boolean;
      Backup      : Map;
      Query_Match : constant Match :=
        Match_Query_Pattern (Ctx, Query.F_Pattern, Node);
   begin
      if not Query_Match.Success then
         return False;
      end if;

      Backup := Backup_Env (Ctx.Env, Query_Match.Bindings);
      Update_Env (Ctx.Env, Query_Match.Bindings);

      Result := Bool_Val (Typed_Eval (Ctx, Query.F_Predicate, Kind_Bool));
      Update_Env (Ctx.Env, Backup);

      return Result;
   exception
      when others =>
         Update_Env (Ctx.Env, Backup);
         raise;
   end Match_Filtered_Query;

   ----------------
   -- Backup_Env --
   ----------------

   function Backup_Env (Parent_Env  : String_Value_Maps.Map;
                        Local_Env  : String_Value_Maps.Map)
                        return String_Value_Maps.Map
   is
      use String_Value_Maps;
      Backup : Map;
   begin
      for C in Local_Env.Iterate loop
         if Parent_Env.Contains (Key (C)) then
            Backup.Insert (Key (C), Element (C));
         end if;
      end loop;

      return Backup;
   end Backup_Env;

   ----------------
   -- Update_Env --
   ----------------

   procedure Update_Env (Env         : in out String_Value_Maps.Map;
                         New_Values  : String_Value_Maps.Map)
   is
      use String_Value_Maps;
   begin
      for C in New_Values.Iterate loop
         Env.Include (Key (C), Element (C));
      end loop;
   end Update_Env;

   -------------------------
   -- Match_Query_Pattern --
   -------------------------

   function Match_Query_Pattern (Ctx           : Eval_Context_Ptr;
                                 Query_Pattern : LEL.Query_Pattern;
                                 Node          : LAL.Ada_Node) return Match
   is
   begin
      return (case Query_Pattern.Kind is
                 when LELCO.lkql_Node_Query_Pattern =>
                   Match_Node_Pattern
                     (Query_Pattern.As_Node_Query_Pattern.F_Queried_Node,
                      Node),
                 when LELCO.lkql_Full_Query_Pattern =>
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
      Node          : LAL.Ada_Node) return Match
   is
      use String_Value_Maps;
      Queried_Match          : constant Match :=
        Match_Node_Pattern (Query_Pattern.F_Queried_Node, Node);
      Related_Nodes_Iter     : Node_Iterator'Class :=
        Make_Selector_Iterator
          (Ctx, Node, Query_Pattern.F_Selector.As_Named_Selector);
      Related_Nodes_Consumer : Exists_Consumer :=
        (Pattern => Query_Pattern.F_Related_Node);
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
                 when LELCO.lkql_Kind_Node_Pattern =>
                   Match_Kind_Node_Pattern
                     (Node_Pattern.As_Kind_Node_Pattern, Node),
                 when LELCO.lkql_Binding_Node_Pattern =>
                   Match_Binding_Node_Pattern
                     (Node_Pattern.As_Binding_Node_Pattern, Node),
                 when LELCO.lkql_Full_Node_Pattern =>
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
        To_Unbounded_Text (Node_Pattern.F_Binding.Text);
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

   ------------------
   -- Binding_Name --
   ------------------

   function Binding_Name
     (Node : LEL.Node_Pattern) return Unbounded_Text_Type
   is
   begin
      return (case Node.Kind is
              when LELCO.lkql_Binding_Node_Pattern =>
                To_Unbounded_Text
                  (Node.As_Binding_Node_Pattern.F_Binding.Text),
              when LELCO.lkql_Full_Node_Pattern =>
                To_Unbounded_Text
                  (Node.As_Full_Node_Pattern.F_Binding_Pattern.F_Binding.Text),
              when others =>
                To_Unbounded_Text (""));
   end Binding_Name;

   ----------------------------
   -- Make_Selector_Iterator --
   ----------------------------

   function Make_Selector_Iterator (Ctx              : Eval_Context_Ptr;
                                    Queried_Node     : LAL.Ada_Node;
                                    Selector_Pattern : LEL.Named_Selector)
                                    return Node_Iterator'Class
   is
      Selector_Name : constant String :=
        To_UTF8 (Selector_Pattern.F_Name.Text);
   begin
      if Selector_Name = "children" then
         return Node_Iterator'Class (Make_Travers_Wrapper (Queried_Node));
      else
         Raise_Invalid_Selector_Name (Ctx, Selector_Pattern);
      end if;
   end Make_Selector_Iterator;

   ----------
   -- Next --
   ----------

   overriding function Next (Iter : in out Traverse_Wrapper;
                             Result : out LAL.Ada_Node) return Boolean
   is
   begin
      return Iter.Inner.Next (Result);
   end Next;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Traverse_Wrapper) is
   begin
      Free_Traverse_Iterator (Iter.Inner);
   end Release;

   ---------------------------
   -- Make_Traverse_Wrapper --
   ---------------------------

   function Make_Travers_Wrapper
     (Root : LAL.Ada_Node) return Traverse_Wrapper
   is
      Iter : constant Traverse_Iterator_Access :=
        new Libadalang.Iterators.Traverse_Iterator'Class'
          (Libadalang.Iterators.Traverse (Root));
   begin
      return Traverse_Wrapper'(Inner => Iter);
   end Make_Travers_Wrapper;

   -------------
   -- Consume --
   -------------

   function Consume (Self : in out Exists_Consumer;
                     Iter : in out Node_Iterator'Class)
                     return Match
   is
      use String_Value_Maps;
      Bindings      : Map;
      Current_Node  : LAL.Ada_Node;
      Current_Match : Match;
      Matched       : Boolean := False;
      Nodes         : constant Primitive := Make_Empty_List (Kind_Node);
      Save_Bindings : constant Boolean :=
        Self.Pattern.Kind = LELCO.lkql_Binding_Node_Pattern or else
        Self.Pattern.Kind = LELCO.lkql_Full_Node_Pattern;
   begin
      while Iter.Next (Current_Node) loop
         Current_Match := Match_Node_Pattern (Self.Pattern, Current_Node);

         if Current_Match.Success then
            Matched := True;

            if Save_Bindings then
               Append (Nodes, To_Primitive (Current_Node));
            end if;
         end if;
      end loop;

      Iter.Release;

      if not Matched then
         return Match_Failure;
      end if;

      if Save_Bindings then
         Bindings.Insert (Binding_Name (Self.Pattern), Nodes);
      end if;

      return (Success => True, Bindings => Bindings);
   end Consume;

end Query;
