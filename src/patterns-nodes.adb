with Custom_Selectors; use Custom_Selectors;
with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;
with Patterns.Match;             use Patterns.Match;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Assertions; use Ada.Assertions;

package body Patterns.Nodes is

   ----------------
   -- Match_Node --
   ----------------

   function Match_Node (Ctx     : Eval_Context;
                        Pattern : L.Node_Pattern;
                        Value   : Primitive) return Match_Result
   is
      Node : LAL.Ada_Node;
   begin
      if not (Kind (Value) = Kind_Node) then
         Raise_Invalid_Kind (Ctx, Pattern.As_LKQL_Node, Kind_Node, Value);
      end if;

      Node := Node_Val (Value);

      return (case Pattern.Kind is
                 when LCO.LKQL_Kind_Node_Pattern =>
                    Match_Kind (Pattern.As_Kind_Node_Pattern, Node),
                 when LCO.LKQL_Relational_Node_Pattern =>
                   Match_Relationnal
                     (Ctx, Pattern.As_Relational_Node_Pattern, Node),
                 when others =>
                    raise Assertion_Error with
                      "Not a node pattern kind: " & L.Kind_Name (Pattern));
   end Match_Node;

   -----------------------
   -- Match_Relationnal --
   -----------------------

   function Match_Relationnal (Ctx     : Eval_Context;
                               Pattern : L.Relational_Node_Pattern;
                               Node    : LAL.Ada_Node'Class)
                               return Match_Result
   is
      Queried_Match          : constant Match_Result :=
        Match_Unfiltered
          (Ctx, Pattern.F_Queried_Node, To_Primitive (Node.As_Ada_Node));
      Related_Nodes_Iter     : Depth_Node_Iter'Class :=
        Make_Selector_Iterator
          (Ctx, Node, Pattern.F_Selector);
      Related_Nodes_Consumer : Node_Consumer'Class :=
        Make_Selector_Consumer
          (Ctx, Pattern.F_Selector, Pattern.F_Related_Node);
      Selector_Match         : Match_Result;
      Bindings               : Environment_Map;
   begin
      if not Queried_Match.Success then
         Related_Nodes_Iter.Release;
         return Match_Failure;
      end if;

      Add_Bindings (Bindings, Queried_Match.Bindings);
      Selector_Match := Related_Nodes_Consumer.Consume (Related_Nodes_Iter);

      if not Selector_Match.Success then
         return Match_Failure;
      end if;

      Add_Bindings (Bindings, Selector_Match.Bindings);

      return (Success => True, Bindings => Bindings);
   end Match_Relationnal;

   ----------------
   -- Match_Kind --
   ----------------

   function Match_Kind (Pattern : L.Kind_Node_Pattern;
                        Node    : LAL.Ada_Node'Class) return Match_Result
   is
      Kind_Name : constant String := To_UTF8 (Pattern.F_Identifier.Text);
   begin
      return Match_Result'
        (Success => Matches_Kind_Name (Kind_Name, Node.As_Ada_Node),
         others => <>);
   end Match_Kind;

   ----------------------------
   -- Make_Selector_Iterator --
   ----------------------------

   function Make_Selector_Iterator
     (Ctx              : Eval_Context;
      Queried_Node     : LAL.Ada_Node'Class;
      Selector_Pattern : L.Selector_Pattern'Class)
      return Depth_Node_Iter'Class
   is
      Base_Selector : constant Depth_Node_Iter'Class :=
        Selector_Iterator_From_Name
          (Ctx, Queried_Node.As_Ada_Node, Selector_Pattern);
   begin
      if Selector_Pattern.P_Condition.Is_Null then
         return Base_Selector;
      end if;

      return
        Depth_Node_Iters.Filter
         (Base_Selector,
          Selector_Conditions_Predicate'(Ctx, Selector_Pattern.P_Condition));
   end Make_Selector_Iterator;

   ---------------------------------
   -- Selector_Iterator_From_Name --
   ---------------------------------

   function Selector_Iterator_From_Name
     (Ctx              : Eval_Context;
      Queried_Node     : LAL.Ada_Node;
      Selector_Pattern : L.Selector_Pattern'Class)
      return Depth_Node_Iter'Class
   is
   begin
      if Selector_Pattern.P_Selector_Name = "children" then
         return Make_Childs_Iterator (Queried_Node);
      else
         declare
            Id           : constant L.Identifier :=
              Selector_Pattern.P_Selector_Identifier;
            Selector_Def : constant L.Selector_Def :=
              Selector_Val (Eval (Ctx, Id, Expected_Kind => Kind_Selector));
            Iter         : constant Custom_Selector_Iter :=
              Make_Custom_Selector_Iter (Ctx, Selector_Def, Queried_Node);
         begin
            return Iter;
         end;
      end if;
   end Selector_Iterator_From_Name;

   ----------------------------
   -- Make_Selector_Consumer --
   ----------------------------

   function Make_Selector_Consumer (Ctx          : Eval_Context;
                                    Selector     : L.Selector_Pattern;
                                    Related_Node : L.Unfiltered_Pattern)
                                    return Node_Consumer'Class
   is
      Quantifier_Name : constant Text_Type :=
        Selector.P_Quantifier_Name;
   begin
      if Quantifier_Name /= "some" and then Quantifier_Name /= "all" then
         Raise_Invalid_Selector_Name (Ctx, Selector);
      end if;

      return (if Quantifier_Name = "some"
              then Exists_Consumer'(Ctx, Related_Node)
              else All_Consumer'(Ctx, Related_Node));
   end Make_Selector_Consumer;

   -------------
   -- Consume --
   -------------

   function Consume (Self : in out Exists_Consumer;
                     Iter : in out Depth_Node_Iter'Class)
                     return Match_Result
   is
      Bindings      : Environment_Map;
      Current_Node  : Depth_Node;
      Current_Match : Match_Result;
      Matched       : Boolean := False;
      Nodes         : constant Primitive := Make_Empty_List;
      Save_Bindings : constant Boolean := Self.Pattern.P_Has_Binding;
   begin
      while Iter.Next (Current_Node) loop
         Current_Match := Match_Unfiltered
           (Self.Ctx, Self.Pattern, To_Primitive (Current_Node.Node));

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
                     Iter : in out Depth_Node_Iter'Class)
                     return Match_Result
   is
      use String_Value_Maps;
      Current_Node  : Depth_Node;
      Current_Match : Match_Result;
      Bindings      : Map;
      Nodes         : constant Primitive := Make_Empty_List;
      Save_Bindings : constant Boolean := Self.Pattern.P_Has_Binding;
   begin
      while Iter.Next (Current_Node) loop
         Current_Match := Match_Unfiltered
           (Self.Ctx, Self.Pattern, To_Primitive (Current_Node.Node));

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
      Element : Depth_Node)
      return Boolean
   is
      Local_Env   : Environment_Map;
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

   ----------
   -- Next --
   ----------

   overriding function Next (Iter : in out Childs_Iterator;
                             Element : out Depth_Node)
                             return Boolean
   is
      Result : constant Option := Pop (Iter.Stack);
   begin
      if Is_None (Result) then
         return False;
      end if;

      Element := Extract (Result);
      Stack_Childs (Iter, Element);
      return True;
   end Next;

   --------------------------
   -- Make_Childs_Iterator --
   --------------------------

   function Make_Childs_Iterator (Node : LAL.Ada_Node) return Childs_Iterator
   is
   begin
      return Result : Childs_Iterator do
         Result.Stack := new Depth_Node_Vectors.Vector;
         Result.Stack.Append ((0, Node));
      end return;
   end Make_Childs_Iterator;

   ------------------
   -- Stack_Childs --
   ------------------

   procedure Stack_Childs
     (Iter : in out Childs_Iterator; Element : Depth_Node)
   is
      Children : LAL.Ada_Node_Array renames Element.Node.Children;
   begin
      for I in reverse Children'Range loop
         if not Children (I).Is_Null then
            Iter.Stack.Append ((Element.Depth + 1, Children (I)));
         end if;
      end loop;
   end Stack_Childs;

   ---------
   -- Pop --
   ---------

   function Pop
     (Stack  : Element_Vector_Access) return Option
   is
      use Depth_Node_Vectors;
      Result        : Depth_Node;
      Result_Cursor : constant Cursor := Stack.all.Last;
   begin
      if not Has_Element (Result_Cursor) then
         return None;
      end if;

      Result := Element (Result_Cursor);
      Stack.all.Delete_Last;
      return To_Option (Result);
   end Pop;

   -------------
   -- Release --
   -------------

   procedure Release (Iter : in out Childs_Iterator) is
   begin
      Free_Element_Vector (Iter.Stack);
   end Release;

   -----------
   -- Clone --
   -----------

   function Clone (Iter : Childs_Iterator) return Childs_Iterator is
      Stack_Copy : constant Element_Vector_Access :=
        new Depth_Node_Vectors.Vector'(Iter.Stack.all);
   begin
      return Childs_Iterator'(Stack => Stack_Copy);
   end Clone;

end Patterns.Nodes;
