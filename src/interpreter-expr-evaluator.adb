package body Interpreter.Expr.Evaluator is

   function Eval (Ctx: in out EvalCtx; Node: in LEL.lkql_Node'Class) return Atom is
   begin
      case Node.Kind is
         when LELCO.lkql_lkql_Node_List => return Eval_List (Ctx, Node.As_lkql_Node_List);
         when LELCO.lkql_Assign => return Eval_Assign (Ctx, Node.As_Assign);
         when LELCO.lkql_Identifier => return Eval_Identifier (Ctx, Node.As_Identifier);
         when LELCO.lkql_Number => return Eval_Number (Node.As_Number);
         when LELCO.lkql_Integer => return Eval_Integer (Node.As_Integer);
         when LELCO.lkql_Print_Stmt => return Eval_Print (Ctx, Node.As_Print_Stmt);
         when LELCO.lkql_String_Literal => return Eval_String_Literal (Node.As_String_Literal);
         when LELCO.lkql_Bin_Op => return Eval_Bin_Op (Ctx, Node.As_Bin_Op);
         when others =>
            raise EvalError with "Unsupported evaluation root: " & Node.Kind_Name;
      end case;
   end Eval;
   
   
   function Eval_List (Ctx: in out EvalCtx; Node: in LEL.lkql_Node_List) return Atom is
      Ignore: Atom := (Kind => Unit);
   begin
      if Node.Children'Length = 0 then
         return (Kind => Unit);
      end if;
      for Child of Node.Children (Node.Children'First .. Node.Children'Last - 1) loop
         Ignore := Eval (Ctx, Child);
      end loop;
      return Eval (Ctx, Node.Children (Node.Children'last));
   end Eval_List;
   
   
   function Eval_Assign (Ctx: in out EvalCtx; Node: in LEL.Assign) return Atom is
      Identifier: constant Unbounded_Text_Type := To_Unbounded_Text (Node.F_Identifier.Text);
   begin
      Ctx.Env.Include (Identifier, Eval (Ctx, Node.F_Value));
      return (Kind => Unit);
   end Eval_Assign;
   
   
   function Eval_Identifier (Ctx: in out EvalCtx; Node: in LEL.Identifier) return Atom is
   begin
      return Ctx.Env (To_Unbounded_Text (Node.Text));
   end Eval_Identifier;
   
   
   function Eval_Number (Node: in LEL.Number) return Atom is
   begin
      return (Kind => Number, Number_Val => Float'Wide_Wide_Value (Node.Text));
   end Eval_Number;
   
   
   function Eval_Integer (Node: in LEL.Integer) return Atom is
   begin
      return (Kind => Int, Int_Val => Integer'Wide_Wide_Value (Node.Text));
   end Eval_Integer;
   
   
   function Eval_String_Literal (Node: in LEL.String_Literal) return Atom is
      Literal: constant Unbounded_Text_Type := To_Unbounded_Text (Node.Text);
   begin
      return (Kind => Str, Str_Val => Unbounded_Slice ( Literal, 2, Length (Literal) - 1));
   end Eval_String_Literal;
   
   
   function Eval_Print (Ctx: in out EvalCtx; Node: in LEL.Print_Stmt) return Atom is
   begin
      Display (Eval (Ctx, Node.F_Value));
      return (Kind => Unit);
   end Eval_Print;
   
   
   function Eval_Bin_Op (Ctx: in out EvalCtx; Node: in LEL.Bin_Op) return Atom is
      Left: constant Atom := Eval (Ctx, Node.F_Left);
      Right: constant Atom := Eval (Ctx, Node.F_Right); 
   begin
      case Node.F_Op is
         when LELCO.lkql_Op_Plus => return Left + Right;
         when LELCO.lkql_Op_Eq => return Left = Right;
         when LELCO.lkql_Op_And => return Left and Right;
         when LELCO.lkql_Op_Or => return Left or Right;
         when others => raise EvalError with "Operator not implemented: " & Node.F_Op.Kind_Name; 
      end case;
   end Eval_Bin_Op;

end Interpreter.Expr.Evaluator;
