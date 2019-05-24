with LKQL.Node_Data;
with LKQL.Patterns.Match;
with LKQL.Custom_Selectors; use LKQL.Custom_Selectors;
with LKQL.Primitives;       use LKQL.Primitives;
with LKQL.Evaluation;       use LKQL.Evaluation;

with Libadalang.Introspection;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body LKQL.Patterns.Nodes is

   ------------------------
   -- Match_Node_Pattern --
   ------------------------

   function Match_Node_pattern (Ctx     : Eval_Context;
                                Pattern : L.Node_Pattern;
                                Node    : LAL.Ada_Node) return Match_Result
   is
   begin
      case Pattern.Kind is
         when LCO.LKQL_Node_Kind_Pattern =>
            return Match_Kind_pattern
              (Ctx, Pattern.As_Node_Kind_Pattern, Node);
         when LCO.LKQL_Extended_Node_Pattern =>
            return Match_Extended_Pattern
              (Ctx, Pattern.As_Extended_Node_Pattern, Node);
         when others =>
            raise Assertion_Error
              with "Invalid node pattern kind: " & L.Kind_Name (Pattern);
      end case;
   end Match_Node_pattern;

   ------------------------
   -- Match_Kind_Pattern --
   ------------------------

   function Match_Kind_pattern (Ctx     : Eval_Context;
                                Pattern : L.Node_Kind_Pattern;
                                Node    : LAL.Ada_Node) return Match_Result
   is
     (if Matches_Kind_Name (To_UTF8 (Pattern.F_Kind_Name.Text), Node)
      then Make_Match_Success
      else Match_Failure);

   ----------------------------
   -- Match_Extended_Pattern --
   ----------------------------

   function Match_Extended_Pattern (Ctx     : Eval_Context;
                                    Pattern : L.Extended_Node_Pattern;
                                    Node    : LAL.Ada_Node)
                                    return Match_Result
   is
      use LKQL.Patterns.Match;
   begin
      if not
        Match_Value (Ctx, Pattern.F_Node_Pattern, To_Primitive (Node)).Success
      then
         return Match_Failure;
      end if;

      return Match_Pattern_Details (Ctx, Pattern.F_Details, Node);
   end Match_Extended_Pattern;

   ---------------------------
   -- Match_Pattern_Details --
   ---------------------------

   function Match_Pattern_Details (Ctx     : Eval_Context;
                                   Details : L.Node_Pattern_Detail_List;
                                   Node    : LAL.Ada_Node)
                                   return Match_Result
   is
      use String_Value_Maps;
      Bindings      : Environment_Map;
      Current_Match : Match_Result;
   begin
      for D of Details loop
         Current_Match := Match_Pattern_Detail (Ctx, Node, D);

         if not Current_Match.Success then
            return Match_Failure;
         end if;

         if not Current_Match.Bindings.Is_Empty then
            for C in Current_Match.Bindings.Iterate loop
               Bindings.Insert (Key (C), Element (C));
            end loop;
         end if;
      end loop;

      return Make_Match_Success (Bindings);
   end Match_Pattern_Details;

   --------------------------
   -- Match_Pattern_Detail --
   --------------------------

   function Match_Pattern_Detail (Ctx    : Eval_Context;
                                  Node   : LAL.Ada_Node;
                                  Detail : L.Node_Pattern_Detail'Class)
                                  return Match_Result
   is
   begin
      case Detail.Kind is
         when LCO.LKQL_Node_Pattern_Data =>
            return
              (if Match_Pattern_Data (Ctx, Node, Detail.As_Node_Pattern_Data)
               then Make_Match_Success
               else Match_Failure);
         when LCO.LKQL_Node_Pattern_Selector =>
            return Match_Pattern_Selector
              (Ctx, Node, Detail.As_Node_Pattern_Selector);
         when others =>
            raise Assertion_Error
              with "Invalid pattern detail kind: " & L.Kind_Name (Detail);
      end case;
   end Match_Pattern_Detail;

   ------------------------
   -- Match_Pattern_Data --
   ------------------------

   function Match_Pattern_Data (Ctx    : Eval_Context;
                                Node   : LAL.Ada_Node;
                                Detail : L.Node_Pattern_Data)
                                return Boolean
   is
      use LKQL.Node_Data;
      Expected_Value : constant Primitive := Eval (Ctx, Detail.F_Value_Expr);
      Data_Value     : constant Primitive :=
        (if Is_Field_Name (Node, Detail.F_Identifier.Text)
         then Access_Node_Field (Ctx, Node, Detail.F_Identifier)
         else Eval_Node_Property
           (Ctx, Node, Detail.F_Identifier, Detail.F_Arguments));
   begin
      return Deep_Equals (Data_Value, Expected_Value);
   end Match_Pattern_Data;

   ----------------------------
   -- Match_Pattern_Selector --
   ----------------------------

   function Match_Pattern_Selector (Ctx      : Eval_Context;
                                    Node     : LAL.Ada_Node;
                                    Selector : L.Node_Pattern_Selector)
                                    return Match_Result
   is
      S_List       : Selector_List;
      Bindings     : Environment_Map;
      Binding_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Selector.F_Call.P_Binding_Name);
   begin
      if not Eval_Selector
        (Ctx, Node, Selector.F_Call, Selector.F_Pattern, S_List)
      then
         return Match_Failure;
      end if;

      if Length (Binding_Name) /= 0 then
         Bindings.Include (Binding_Name, To_Primitive (S_List));
      end if;

      return Make_Match_Success (Bindings);
   end Match_Pattern_Selector;

   -------------------
   -- Eval_Selector --
   -------------------

   function Eval_Selector (Ctx     : Eval_Context;
                           Node    : LAL.Ada_Node;
                           Call    : L.Selector_Call;
                           Pattern : L.Base_Pattern;
                           Result  : out Selector_List) return Boolean
   is
      Quantifier_Name   : constant String :=
        To_UTF8 (Call.P_Quantifier_Name);
      Selector_Iterator : constant Depth_Node_Iter_Access :=
        new Depth_Node_Iter'Class'
          (Depth_Node_Iter'Class
             (Make_Custom_Selector_Iter (Ctx, Call, Node)));
      Pattern_Predicate : constant Depth_Node_Iters.Predicate_Access :=
        new Depth_Node_Iters.Predicates.Func'Class'
          (Depth_Node_Iters.Predicates.Func'Class
             (Make_Node_Pattern_Predicate (Ctx, Pattern)));
      Filtered_Iter     : constant Depth_Node_Filter_Access :=
        new Depth_Node_Iters.Filter_Iter'
          (Depth_Node_Iters.Filter (Selector_Iterator, Pattern_Predicate));
   begin
      return Make_Selector_List (Filtered_Iter, Quantifier_Name, Result);
   end Eval_Selector;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Self : in out Node_Pattern_Predicate; Node : Depth_Node) return Boolean
   is
      use LKQL.Patterns.Match;
      Result : constant Match_Result :=
        Match_Pattern (Self.Ctx, Self.Pattern, To_Primitive (Node.Node));
   begin
      return Result.Success;
   end Evaluate;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Self : Node_Pattern_Predicate) return Node_Pattern_Predicate
   is
     (Self.Ctx.Clone_Frame, Self.Pattern);

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Node_Pattern_Predicate) is
   begin
      Self.Ctx.Release_Current_Frame;
   end Release;

   ---------------------------------
   -- Make_Node_Pattern_Predicate --
   ---------------------------------

   function Make_Node_Pattern_Predicate (Ctx        : Eval_Context;
                                         Pattern    : L.Base_Pattern)
                                         return Node_Pattern_Predicate
   is
      (Ctx.Clone_Frame, Pattern);

   -----------------------
   -- Matches_Type_Name --
   -----------------------

   function Matches_Kind_Name
     (Kind_Name : String; Node : LAL.Ada_Node) return Boolean
   is
      use Libadalang.Introspection;
      Expected_Kind : constant Any_Node_Type_Id :=
        Lookup_DSL_Name (Kind_Name);
      Actual_Kind   : constant Any_Node_Type_Id :=
        Id_For_Kind (Node.Kind);
   begin
      pragma Assert
        (Expected_Kind /= None, "Invalid kind name: " & Kind_Name);

      return Actual_Kind = Expected_Kind or else
             Is_Derived_From (Actual_Kind, Expected_Kind);
   end Matches_Kind_Name;

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

end LKQL.Patterns.Nodes;
