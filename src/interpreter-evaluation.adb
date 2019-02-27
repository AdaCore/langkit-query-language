with Interpreter.Types.Node_Lists; use Interpreter.Types.Node_Lists;

with Libadalang.Introspection; use Libadalang.Introspection;
with Libadalang.Iterators; use Libadalang.Iterators;

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
         when LELCO.lkql_Integer =>
            return Eval_Integer (Node.As_Integer);
         when LELCO.lkql_Print_Stmt =>
            return Eval_Print (Ctx, Node.As_Print_Stmt);
         when LELCO.lkql_String_Literal =>
            return Eval_String_Literal (Node.As_String_Literal);
         when LELCO.lkql_Bin_Op =>
            return Eval_Bin_Op (Ctx, Node.As_Bin_Op);
         when LELCO.lkql_Is_Clause =>
            return Eval_Is (Ctx, Node.As_Is_Clause);
         when LELCO.lkql_Query =>
            return Eval_Query (Ctx, Node.As_Query);
         when others =>
            raise Eval_Error
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

   ------------------
   -- Eval_integer --
   ------------------

   function Eval_Integer (Node : LEL.Integer) return Primitive is
   begin
      return To_Primitive (Integer'Wide_Wide_Value (Node.Text));
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
      return To_Primitive (Literal);
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
   -- Eval_Dot_Acess --
   --------------------

   function Eval_Dot_Access
     (Ctx : in out Eval_Context; Node : LEL.Dot_Access) return Primitive
   is
      Receiver    : constant Primitive := Eval (Ctx, Node.F_Receiver);
      Member_Name : constant Text_Type := Node.F_Member.Text;
   begin
      if Receiver.Kind /= Kind_Node then
         raise Eval_Error with
           "Cannot get member " & To_UTF8 (Member_Name)
             & " of node of kind " & Kind_Name (Receiver);
      end if;

      return To_Primitive (Get_Field (Member_Name, Receiver.Node_Val));
   end Eval_Dot_Access;

   -------------
   -- Eval Is --
   -------------

   function Eval_Is
     (Ctx : in out Eval_Context; Node : LEL.Is_Clause) return Primitive
   is
      Tested_Node : constant Primitive := Eval (Ctx, Node.F_Identifier);
   begin
      if Tested_Node.Kind /= Kind_Node then
         raise Eval_Error with
           "A node of kind " & Kind_Name (Tested_Node)
             & " cannot be on the left side of an is clause";
      end if;

      declare
         Expected_Kind : constant String := To_UTF8 (Node.F_Kind_Name.Text);
         Kind_Match    : constant Boolean :=
           Tested_Node.Node_Val.Kind_Name = Expected_Kind;
      begin
         return To_Primitive (Kind_Match);
      end;
   end Eval_Is;

   ----------------
   -- Eval_Query --
   ----------------

   function Eval_Query
     (Ctx : in out Eval_Context; Node : LEL.Query) return Primitive
   is
      It           : Traverse_Iterator'Class := Traverse (Ctx.AST_Root);
      Current_Node : LAL.Ada_Node;
      Result       : Node_List;
      Local_Ctx    : Eval_Context;
      Binding      : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node.F_Binding.Text);
   begin
      while It.Next (Current_Node) loop
         Local_Ctx := Ctx;
         Local_Ctx.Env.Include (Binding, To_Primitive (Current_Node));
         if Eval (Local_Ctx, Node.F_When_Clause) = To_Primitive (True) then
            Result.Nodes.Append (Current_Node);
         end if;
      end loop;

      return (Kind => Kind_Node_List, Node_List_Val => Result);
   end Eval_Query;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field
     (Name : Text_Type; Node : LAL.Ada_Node) return LAL.Ada_Node
   is
      Idx : constant Positive := Get_Field_Index (Name, Node);
   begin
      return Node.Children (Idx);
   end Get_Field;

   ---------------------
   -- Get_Field_Index --
   ---------------------

   function Get_Field_Index
     (Name : Text_Type; Node : LAL.Ada_Node) return Positive
   is
      UTF8_Name : constant String := To_UTF8 (Name);
   begin
      for F of Fields (Node.Kind) loop
         if Field_Name (F) = UTF8_Name then
            return Index (Node.Kind, F);
         end if;
      end loop;

      raise Eval_Error with
        "Node of kind " & Node.Kind_Name & " has no field named " & UTF8_Name;
   end Get_Field_Index;

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
            raise Eval_Error with "Operator not implemented: " & Op.Kind_Name;
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
         raise Eval_Error
           with "Node of kind " & Node.Kind_Name &
           " cannot be reduced to an atom.";
      else
         return Reduced.Atom_Val;
      end if;
   end Reduce;

end Interpreter.Evaluation;
