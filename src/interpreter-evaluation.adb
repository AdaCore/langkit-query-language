package body Interpreter.Evaluation is

   ----------
   -- Eval --
   ----------

   function Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Primitive
   is
   begin
      case Node.Kind is
         when LELCO.lkql_LKQL_Node_List =>
            return Eval_List (Ctx, Node.As_LKQL_Node_List);
         when LELCO.lkql_Assign =>
            return Eval_Assign (Ctx, Node.As_Assign);
         when LELCO.lkql_Identifier =>
            return Eval_Identifier (Ctx, Node.As_Identifier);
         when LELCO.lkql_Number =>
            return Eval_Number (Node.As_Number);
         when LELCO.lkql_Integer =>
            return Eval_Integer (Node.As_Integer);
         when LELCO.lkql_Print_Stmt =>
            return Eval_Print (Ctx, Node.As_Print_Stmt);
         when LELCO.lkql_String_Literal =>
            return Eval_String_Literal (Node.As_String_Literal);
         when LELCO.lkql_Bin_Op =>
            return Eval_Bin_Op (Ctx, Node.As_Bin_Op);
         when others =>
            raise EvalError
              with "Unsupported evaluation root: " & Node.Kind_Name;
      end case;
   end Eval;

   ---------------
   -- Eval_List --
   ---------------

   function Eval_List
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node_List) return Primitive
   is
      Result : Primitive;
   begin
      if Node.Children'Length = 0 then
         return To_Primitive ((Kind => Kind_Unit));
      end if;

      for Child of Node.Children loop
         Result := Eval (Ctx, Child);
      end loop;

      return Result;
   end Eval_List;

   -----------------
   -- Eval_Assign --
   -----------------

   function Eval_Assign
     (Ctx : in out Eval_Context; Node : LEL.Assign) return Primitive
   is
      Identifier : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node.F_Identifier.Text);
   begin
      Ctx.Env.Include (Identifier, Eval (Ctx, Node.F_Value));
      return To_Primitive ((Kind => Kind_Unit));
   end Eval_Assign;

   ---------------------
   -- Eval_identifier --
   ---------------------

   function Eval_Identifier
     (Ctx : in out Eval_Context; Node : LEL.Identifier) return Primitive
   is
   begin
      return Ctx.Env (To_Unbounded_Text (Node.Text));
   end Eval_Identifier;

   -----------------
   -- Eval_Number --
   -----------------

   function Eval_Number (Node : LEL.Number) return Primitive is
   begin
      return To_Primitive
          ((Kind       => Kind_Number,
            Number_Val => Float'Wide_Wide_Value (Node.Text)));
   end Eval_Number;

   ------------------
   -- Eval_integer --
   ------------------

   function Eval_Integer (Node : LEL.Integer) return Primitive is
   begin
      return To_Primitive
          ((Kind => Kind_Int, Int_Val => Integer'Wide_Wide_Value (Node.Text)));
   end Eval_Integer;

   -------------------------
   -- Eval_String_Literal --
   -------------------------

   function Eval_String_Literal (Node : LEL.String_Literal) return Primitive is
      Quoted_Literal : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node.Text);
      Literal : constant Unbounded_Text_Type :=
        Unbounded_Slice (Quoted_Literal, 2, Length (Quoted_Literal) - 1);
   begin
      return To_Primitive ((Kind => Kind_Str, Str_Val => Literal));
   end Eval_String_Literal;

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print
     (Ctx : in out Eval_Context; Node : LEL.Print_Stmt) return Primitive
   is
   begin
      Display (Eval (Ctx, Node.F_Value));
      return To_Primitive ((Kind => Kind_Unit));
   end Eval_Print;

   -----------------
   -- Eval_Bin_Op --
   -----------------

   function Eval_Bin_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
      Left   : constant Atom := Reduce (Ctx, Node.F_Left);
      Right  : constant Atom := Reduce (Ctx, Node.F_Right);
      Result : constant Atom := Compute_Bin_Op (Node.F_Op, Left, Right);
   begin
      return To_Primitive (Result);
   end Eval_Bin_Op;

   --------------------
   -- Compute_Bin_Op --
   --------------------

   function Compute_Bin_Op (Op : LEL.Op'Class; Left, Right : Atom) return Atom
   is
   begin
      case Op.Kind is
         when LELCO.lkql_Op_Plus =>
            return Left + Right;
         when LELCO.lkql_Op_Eq =>
            return Left = Right;
         when LELCO.lkql_Op_And =>
            return Left and Right;
         when LELCO.lkql_Op_Or =>
            return Left or Right;
         when others =>
            raise EvalError with "Operator not implemented: " & Op.Kind_Name;
      end case;
   end Compute_Bin_Op;

   ------------
   -- Reduce --
   ------------

   function Reduce
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Atom
   is
      Reduced : constant Primitive := Eval (Ctx, Node);
   begin
      if Reduced.Kind /= Kind_Atom then
         raise EvalError
           with "Node of kind " & Node.Kind_Name &
           " cannot be reduced to an atom.";
      else
         return Reduced.Atom_Val;
      end if;
   end Reduce;

end Interpreter.Evaluation;
