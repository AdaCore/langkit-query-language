with LKQL.Node_Data;
with LKQL.Patterns.Match;   use LKQL.Patterns.Match;
with LKQL.Custom_Selectors; use LKQL.Custom_Selectors;
with LKQL.Primitives;       use LKQL.Primitives;
with LKQL.Evaluation;       use LKQL.Evaluation;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with LKQL.Error_Handling; use LKQL.Error_Handling;

package body LKQL.Patterns.Nodes is

   -----------------------
   -- Filter_Node_Array --
   -----------------------

   function Filter_Node_Array (Ctx     : Eval_Context;
                               Pattern : L.Base_Pattern;
                               Nodes   : AST_Node_Rc_Array)
                               return AST_Node_Rc_Array
   is
      Filtered : AST_Node_Vector;
   begin
      for N of Nodes loop
         if Match_Pattern (Ctx, Pattern, To_Primitive (N)).Is_Success then
            Filtered.Append (N);
         end if;
      end loop;

      return Result : AST_Node_Rc_Array (1 .. Filtered.Last_Index) do
         for I in 1 .. Filtered.Last_Index loop
            Result (I) := Filtered (I);
         end loop;
      end return;
   end Filter_Node_Array;

   ------------------------
   -- Match_Node_Pattern --
   ------------------------

   function Match_Node_pattern (Ctx     : Eval_Context;
                                Pattern : L.Node_Pattern;
                                Node    : AST_Node_Rc) return Match_Result
   is
   begin
      if Node.Get.Is_Null_Node then
         return Match_Failure;
      end if;

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
                                Node    : AST_Node_Rc) return Match_Result
   is
   begin
      return
        (if Node.Get.Matches_Kind_Name (To_UTF8 (Pattern.F_Kind_Name.Text))
         then Make_Match_Success (To_Primitive (Node))
         else Match_Failure);
   exception
      when E : Unsupported_Error =>
         Raise_From_Exception (Ctx, E, Pattern);
   end Match_Kind_pattern;

   ----------------------------
   -- Match_Extended_Pattern --
   ----------------------------

   function Match_Extended_Pattern (Ctx     : Eval_Context;
                                    Pattern : L.Extended_Node_Pattern;
                                    Node    : AST_Node_Rc)
                                    return Match_Result
   is
      Match : constant Match_Result :=
        Match_Value
          (Ctx, Pattern.F_Node_Pattern, To_Primitive (Node));
      Result : constant Match_Result :=
        (if Match.Is_Success
         then Match_Pattern_Details (Ctx, Pattern.F_Details, Node)
         else Match_Failure);
   begin
      return Result;
   end Match_Extended_Pattern;

   ---------------------------
   -- Match_Pattern_Details --
   ---------------------------

   function Match_Pattern_Details (Ctx     : Eval_Context;
                                   Details : L.Node_Pattern_Detail_List;
                                   Node    : AST_Node_Rc)
                                   return Match_Result
   is
      use String_Value_Maps;
      Bindings      : Environment_Map;
      Current_Match : Match_Result;
   begin
      for D of Details loop
         Current_Match := Match_Pattern_Detail (Ctx, Node, D);

         if not Current_Match.Is_Success then
            return Match_Failure;
         end if;

         if not Current_Match.Bindings.Is_Empty then
            for C in Current_Match.Bindings.Iterate loop
               Bindings.Insert (Key (C), Element (C));
            end loop;
         end if;
      end loop;

      return Make_Match_Success (To_Primitive (Node), Bindings);
   end Match_Pattern_Details;

   --------------------------
   -- Match_Pattern_Detail --
   --------------------------

   function Match_Pattern_Detail (Ctx    : Eval_Context;
                                  Node   : AST_Node_Rc;
                                  Detail : L.Node_Pattern_Detail'Class)
                                  return Match_Result
   is
   begin
      case Detail.Kind is
         when LCO.LKQL_Node_Pattern_Field =>
            return Match_Pattern_Field
              (Ctx, Node, Detail.As_Node_Pattern_Field);
         when LCO.LKQL_Node_Pattern_Property =>
            return Match_Pattern_Property
              (Ctx, Node, Detail.As_Node_Pattern_Property);
         when LCO.LKQL_Node_Pattern_Selector =>
            return Match_Pattern_Selector
              (Ctx, Node, Detail.As_Node_Pattern_Selector);
         when others =>
            raise Assertion_Error
              with "Invalid pattern detail kind: " & L.Kind_Name (Detail);
      end case;
   end Match_Pattern_Detail;

   -------------------------
   -- Match_Pattern_Field --
   -------------------------

   function Match_Pattern_Field (Ctx    : Eval_Context;
                                 Node   : AST_Node_Rc;
                                 Field  : L.Node_Pattern_Field)
                                 return Match_Result
   is
      use LKQL.Node_Data;
      Field_Value : constant Primitive :=
        Access_Node_Field (Ctx, Node, Field.F_Identifier);
   begin
      return Match_Detail_Value (Ctx, Field_Value, Field.F_Expected_Value);
   end Match_Pattern_Field;

   ----------------------------
   -- Match_Pattern_Property --
   ----------------------------

   function Match_Pattern_Property (Ctx      : Eval_Context;
                                    Node     : AST_Node_Rc;
                                    Property : L.Node_Pattern_Property)
                                    return Match_Result
   is
      use LKQL.Node_Data;
      Property_Value : constant Primitive :=
        Eval_Node_Property
          (Ctx, Node, Property.F_Call.F_Name, Property.F_Call.F_Arguments);
   begin
      return Match_Detail_Value
        (Ctx, Property_Value, Property.F_Expected_Value);
   end Match_Pattern_Property;

   ----------------------------
   -- Match_Pattern_Selector --
   ----------------------------

   function Match_Pattern_Selector (Ctx      : Eval_Context;
                                    Node     : AST_Node_Rc;
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

      return Make_Match_Success (To_Primitive (Node), Bindings);
   end Match_Pattern_Selector;

   -------------------
   -- Eval_Selector --
   -------------------

   function Eval_Selector (Ctx     : Eval_Context;
                           Node    : AST_Node_Rc;
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
      Result : constant Match_Result :=
        Match_Pattern
          (Self.Ctx, Self.Pattern, To_Primitive (Node.Node));
   begin
      return Result.Is_Success;
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

   ------------------------
   -- Match_Detail_Value --
   ------------------------

   function Match_Detail_Value (Ctx    : Eval_Context;
                                Value  : Primitive;
                                Detail : L.Detail_Value) return Match_Result
   is
      use LCO;
   begin
      if Detail.Kind = LKQL_Detail_Expr then
         declare
            Detail_Value : constant Primitive :=
              Eval (Ctx, Detail.As_Detail_Expr.F_Expr_Value,
                    Expected_Kind => Kind (Value));
         begin
            return (if Deep_Equals (Value, Detail_Value)
                    then Make_Match_Success (Value)
                    else Match_Failure);
         end;
      else
         return Match_Pattern
           (Ctx, Detail.As_Detail_Pattern.F_Pattern_Value, Value);
      end if;
   end Match_Detail_Value;

end LKQL.Patterns.Nodes;
