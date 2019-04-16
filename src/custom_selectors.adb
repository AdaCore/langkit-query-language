with Patterns;                   use Patterns;
with Patterns.Match;             use Patterns.Match;
with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Primitives;     use Interpreter.Primitives;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

package body Custom_Selectors is

   ----------
   -- Next --
   ----------

   function Next (Iter   : in out Custom_Selector_Iter;
                  Result : out Depth_Node) return Boolean
   is
   begin
      Iter.Eval_Selector;

      if Iter.Next_Values.Is_Empty then
         if Iter.Next_To_Visit.Is_Empty then
            return False;
         else
            return Iter.Next (Result);
         end if;
      end if;

      Result := Iter.Next_Values.First_Element;
      Iter.Next_Values.Delete_First;
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   function Clone (Iter : Custom_Selector_Iter) return Custom_Selector_Iter is
      (Iter);

   -------------------------------
   -- Make_Custom_Selector_Iter --
   -------------------------------

   function Make_Custom_Selector_Iter (Ctx        : Eval_Context;
                                       Definition : L.Selector_Def;
                                       Root       : LAL.Ada_Node)
                                       return Custom_Selector_Iter
   is
      Next_Values   : Depth_Node_Lists.List;
      Next_To_Visit : Depth_Node_Lists.List;
      Root_Node     : constant Depth_Node :=
        Depth_Node'(0, Root);
   begin
      Next_Values.Append (Root_Node);
      Next_To_Visit.Append (Root_Node);
      return Custom_Selector_Iter'
        (Ctx, Definition, Next_Values, Next_To_Visit, others => <>);
   end Make_Custom_Selector_Iter;

   -------------------
   -- Eval_Selector --
   -------------------

   procedure Eval_Selector (Iter : in out Custom_Selector_Iter) is
      use Depth_Node_Lists;
      Position : Cursor := Iter.Next_To_Visit.First;
      Node     : Depth_Node;
   begin
      if not Has_Element (Position) then
         return;
      end if;

      Node := Element (Position);
      Iter.Next_To_Visit.Delete (Position);
      Eval_Selector (Iter, Node);
   end Eval_Selector;

   -------------------
   -- Eval_Selector --
   -------------------

   procedure Eval_Selector (Iter : in out Custom_Selector_Iter;
                            Node : Depth_Node)
   is
      Local_Ctx          : Eval_Context;
      Matched_Arm        : L.Selector_Arm;
      Exprs_To_Add       : L.Selector_Expr_List;
      Node_Value         : constant Primitive := To_Primitive (Node.Node);
      Match_Data         : constant Match_Array_Result :=
        Match_Pattern_Array (Iter.Ctx, Iter.Selector.P_Patterns, Node_Value);
   begin
      if Match_Data.Index = 0 then
         return;
      end if;

      Local_Ctx := Iter.Ctx.Create_New_Frame (Match_Data.Bindings);
      Local_Ctx.Add_Binding ("it", Node_Value);

      Matched_Arm :=
        Iter.Selector.F_Arms.Children (Match_Data.Index).As_Selector_Arm;
      Exprs_To_Add := Matched_Arm.F_Value;

      for E of Exprs_To_Add loop
         Add_Selector_Expr
           (Iter, Local_Ctx, Node.Depth, E.As_Selector_Expr);
      end loop;

      Local_Ctx.Release_Current_Frame;
   end Eval_Selector;

   -----------------------
   -- Add_Selector_Expr --
   -----------------------

   procedure Add_Selector_Expr (Iter      : in out Custom_Selector_Iter;
                                Local_Ctx : Eval_Context;
                                Depth     : Natural;
                                Expr      : L.Selector_Expr)
   is
      use type LCO.LKQL_Node_Kind_Type;
      Expr_Value : constant Primitive :=
        (if Expr.F_Expr.Kind = LCO.LKQL_Unpack
         then Eval (Local_Ctx, Expr.F_Expr.As_Unpack.F_Collection_Expr)
         else Eval (Local_Ctx, Expr.F_Expr));
   begin
      if Kind (Expr_Value) = Kind_Node then
         Add_Node (Iter, Depth, Node_Val (Expr_Value), Expr.F_Mode);
      elsif Kind (Expr_Value) in Sequence_Kind and then
        Expr.F_Expr.Kind = LCO.LKQL_Unpack
      then
         for N of List_Val (Expr_Value).Elements loop
            Add_Node (Iter, Depth, Node_Val (N), Expr.F_Mode);
         end loop;
      elsif Kind (Expr_Value) = Kind_Unit then
         return;
      else
         Raise_Invalid_Kind_For_Selector (Local_Ctx, Expr, Expr_Value);
      end if;
   end Add_Selector_Expr;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (Iter  : in out Custom_Selector_Iter;
                       Depth : Natural;
                       Node  : LAL.Ada_Node;
                       Mode  : L.Selector_Expr_Mode)
   is
      use type LCO.LKQL_Node_Kind_Type;
      Depth_Offset : constant Integer :=
        (if Mode.Kind = LCO.LKQL_Selector_Expr_Mode_Skip then 0 else 1);
      With_Depth : constant Depth_Node :=
        Depth_Node'(Depth + Depth_Offset, Node);
   begin
      if Node.Is_Null then
         return;
      end if;

      if Mode.Kind /= LCO.LKQL_Selector_Expr_Mode_Skip then
         Add_If_Unseen (With_Depth, Iter.Already_Yielded, Iter.Next_Values);
      end if;

      if Mode.Kind /= LCO.LKQL_Selector_Expr_Mode_Default then
         Add_If_Unseen (With_Depth, Iter.Already_Visited, Iter.Next_To_Visit);
      end if;
   end Add_Node;

   -------------------
   -- Add_If_Unseen --
   -------------------

   procedure Add_If_Unseen
     (Node        : Depth_Node;
      Cache       : in out Depth_Node_Sets.Set;
      Target_List : out Depth_Node_Lists.List)
   is
   begin
      if Cache.Contains (Node) then
         return;
      end if;

      Cache.Insert (Node);
      Target_List.Append (Node);
   end Add_If_Unseen;

end Custom_Selectors;
