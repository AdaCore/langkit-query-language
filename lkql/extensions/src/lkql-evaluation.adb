------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Assertions;                  use Ada.Assertions;

with Langkit_Support.Text; use Langkit_Support.Text;

with LKQL.Errors;             use LKQL.Errors;
with LKQL.Queries;            use LKQL.Queries;
with LKQL.Patterns;           use LKQL.Patterns;
with LKQL.Functions;          use LKQL.Functions;
with LKQL.Node_Data;
with LKQL.Patterns.Match;     use LKQL.Patterns.Match;
with LKQL.Error_Handling;     use LKQL.Error_Handling;
with LKQL.Adaptive_Integers;  use LKQL.Adaptive_Integers;
with LKQL.Node_Extensions;    use LKQL.Node_Extensions;
with LKQL.Lk_Nodes_Iterators; use LKQL.Lk_Nodes_Iterators;

package body LKQL.Evaluation is

   use all type Unbounded_Text_Type;

   function Eval_List
     (Ctx : Eval_Context; Node : L.Lkql_Node_List) return Primitive;

   function Eval_Val_Decl
     (Ctx : Eval_Context; Node : L.Val_Decl) return Primitive;

   function Eval_Fun_Decl
     (Ctx : Eval_Context; Node : L.Fun_Decl) return Primitive;

   function Eval_Selector_Decl
     (Ctx : Eval_Context; Node : L.Selector_Decl) return Primitive;

   function Eval_Fun_Expr
     (Ctx             : Eval_Context;
      Node            : L.Base_Function;
      With_Call_Cache : Boolean := False) return Primitive;

   function Eval_Identifier
     (Ctx : Eval_Context; Node : L.Identifier) return Primitive;

   function Eval_Tuple (Ctx : Eval_Context; Node : L.Tuple) return Primitive;

   function Eval_Bool_Literal (Node : L.Bool_Literal) return Primitive;

   function Eval_Unit_Literal (Node : L.Unit_Literal) return Primitive;

   function Eval_If_Then_Else
     (Ctx : Eval_Context; Node : L.If_Then_Else) return Primitive;

   function Eval_Bin_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Un_Op
     (Ctx : Eval_Context; Node : L.Un_Op) return Primitive;

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Dot_Access
     (Ctx : Eval_Context; Node : L.Dot_Access) return Primitive;

   function Eval_Safe_Access
     (Ctx  : Eval_Context; Node : L.Safe_Access) return Primitive;

   function Eval_Is
     (Ctx : Eval_Context; Node : L.Is_Clause) return Primitive;

   function Eval_In
     (Ctx : Eval_Context; Node : L.In_Clause) return Primitive;

   function Eval_Query
     (Ctx : Eval_Context; Node : L.Query) return Primitive;

   function Eval_Indexing
     (Ctx : Eval_Context; Node : L.Indexing) return Primitive;

   function Eval_List_Comprehension
     (Ctx : Eval_Context; Node : L.List_Comprehension) return Primitive;

   function Eval_Block_Expr
     (Ctx : Eval_Context; Node : L.Block_Expr) return Primitive;

   function Eval_Body_Decl
     (Ctx : Eval_Context; Node : L.Block_Body_Decl) return Primitive;

   function Eval_Body_Expr
     (Ctx : Eval_Context; Node : L.Block_Body_Expr) return Primitive;

   function Eval_List_Literal
     (Ctx : Eval_Context; Node : L.List_Literal) return Primitive;

   function Eval_Object_Literal
     (Ctx : Eval_Context; Node : L.Object_Literal) return Primitive;

   function Eval_At_Object_Literal
     (Ctx : Eval_Context; Node : L.At_Object_Literal) return Primitive;

   function Eval_Match (Ctx : Eval_Context; Node : L.Match) return Primitive;

   function Eval_Unwrap (Ctx : Eval_Context; Node : L.Unwrap) return Primitive;

   function Eval_Import
     (Ctx : Eval_Context; Node : L.Import) return Primitive;

   function Make_Comprehension_Environment_Iter
     (Ctx : Eval_Context; Node : L.List_Comp_Assoc_List)
      return Comprehension_Env_Iter;
   --  Given a List of Arrow_Assoc, return an iterator that yields the
   --  environments produced by this list of Arrow_Assoc in the context of a
   --  list comprehension.

   function Get_Truthy (Ctx   : Eval_Context;
                        Node  : L.Lkql_Node;
                        Value : Primitive)
                        return Primitive;
   --  Given a primitive, returns its truthy value if it has one.
   --  Raise an exception and register an error in the evaluation context if
   --  the primitive has no truthy value.

   ----------------
   -- Get_Truthy --
   ----------------

   function Get_Truthy (Ctx   : Eval_Context;
                        Node  : L.Lkql_Node;
                        Value : Primitive)
                        return Primitive
   is
      Has_Truthy : Boolean;
      Truthy_Value : constant Boolean := Truthy (Value, Has_Truthy);
   begin
      if Has_Truthy then
         return To_Primitive (Truthy_Value);
      else
         Raise_Invalid_Kind (Ctx, Node, Kind_Bool, Value);
      end if;
   end Get_Truthy;

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind (Ctx           : Eval_Context;
                         Node          : L.Lkql_Node;
                         Expected_Kind : Valid_Primitive_Kind;
                         Value         : Primitive)
   is
   begin
      if Kind (Value) /= Expected_Kind then
         Raise_Invalid_Kind (Ctx, Node, Expected_Kind, Value);
      end if;
   end Check_Kind;

   ----------
   -- Eval --
   ----------

   function Eval (Ctx            : Eval_Context;
                  Node           : L.Lkql_Node'Class;
                  Expected_Kind  : Base_Primitive_Kind := No_Kind;
                  Local_Bindings : Environment_Map :=
                    String_Value_Maps.Empty_Map)
                  return Primitive
   is
      Result             : Primitive;
      Local_Context      : Eval_Context :=
        (if Local_Bindings.Is_Empty then Ctx
         else Ctx.Create_New_Frame (Local_Bindings));
   begin

      case Node.Kind is
         when LCO.Lkql_Lkql_Node_List =>
            Result := Eval_List (Local_Context, Node.As_Lkql_Node_List);
         when LCO.Lkql_Val_Decl =>
            Result := Eval_Val_Decl (Local_Context, Node.As_Val_Decl);

         when LCO.Lkql_Import =>
            Result := Eval_Import (Local_Context, Node.As_Import);
         when LCO.Lkql_Identifier =>
            Result := Eval_Identifier (Local_Context, Node.As_Identifier);
         when LCO.Lkql_Integer_Literal =>
            Result := To_Primitive
              (Adaptive_Integers.Create (To_UTF8 (Node.Text)),
               Local_Context.Pool);
         when LCO.Lkql_Tuple =>
            Result := Eval_Tuple (Local_Context, Node.As_Tuple);
         when LCO.Lkql_String_Literal | LCO.Lkql_Block_String_Literal =>
            Result := To_Primitive
              (Get_Ext (Node).Content.Denoted_Value.all, Local_Context.Pool);
         when LCO.Lkql_Bool_Literal =>
            Result := Eval_Bool_Literal (Node.As_Bool_Literal);
         when LCO.Lkql_Unit_Literal =>
            Result := Eval_Unit_Literal (Node.As_Unit_Literal);
         when LCO.Lkql_If_Then_Else =>
            Result := Eval_If_Then_Else (Local_Context, Node.As_If_Then_Else);
         when LCO.Lkql_Bin_Op_Range =>
            Result := Eval_Bin_Op (Local_Context, Node.As_Bin_Op);
         when LCO.Lkql_Un_Op_Range =>
            Result := Eval_Un_Op (Local_Context, Node.As_Un_Op);
         when LCO.Lkql_Dot_Access =>
            Result := Eval_Dot_Access (Local_Context, Node.As_Dot_Access);
         when LCO.Lkql_Safe_Access =>
            Result := Eval_Safe_Access (Local_Context, Node.As_Safe_Access);
         when LCO.Lkql_Is_Clause =>
            Result := Eval_Is (Local_Context, Node.As_Is_Clause);
         when LCO.Lkql_In_Clause =>
            Result := Eval_In (Local_Context, Node.As_In_Clause);
         when LCO.Lkql_Query =>
            Result := Eval_Query (Local_Context, Node.As_Query);
         when LCO.Lkql_Indexing | LCO.Lkql_Safe_Indexing =>
            Result := Eval_Indexing (Local_Context, Node.As_Indexing);
         when LCO.Lkql_List_Comprehension =>
            Result := Eval_List_Comprehension
              (Local_Context, Node.As_List_Comprehension);
         when LCO.Lkql_Block_Expr =>
            Result := Eval_Block_Expr (Local_Context, Node.As_Block_Expr);
         when LCO.Lkql_Block_Body_Decl =>
            Result :=
              Eval_Body_Decl (Local_Context, Node.As_Block_Body_Decl);
         when LCO.Lkql_Block_Body_Expr =>
            Result :=
              Eval_Body_Expr (Local_Context, Node.As_Block_Body_Expr);
         when LCO.Lkql_Fun_Decl =>
            Result := Eval_Fun_Decl (Local_Context, Node.As_Fun_Decl);
         when LCO.Lkql_Selector_Decl =>
            Result :=
              Eval_Selector_Decl (Local_Context, Node.As_Selector_Decl);
         when LCO.Lkql_Anonymous_Function =>
            Result := Eval_Fun_Expr (Local_Context, Node.As_Base_Function);
         when LCO.Lkql_Fun_Call =>
            Result := Eval_Call (Local_Context, Node.As_Fun_Call);
         when LCO.Lkql_Match =>
            Result := Eval_Match (Local_Context, Node.As_Match);
         when LCO.Lkql_Unwrap =>
            Result := Eval_Unwrap (Local_Context, Node.As_Unwrap);
         when LCO.Lkql_Null_Literal =>
            Result := To_Primitive (LK.No_Lk_Node, Local_Context.Pool);
         when LCO.Lkql_List_Literal =>
            Result := Eval_List_Literal (Local_Context, Node.As_List_Literal);
         when LCO.Lkql_Object_Literal =>
            Result := Eval_Object_Literal
              (Local_Context, Node.As_Object_Literal);
         when LCO.Lkql_At_Object_Literal =>
            Result := Eval_At_Object_Literal
              (Local_Context, Node.As_At_Object_Literal);
         when others =>
            raise Assertion_Error
              with "Invalid evaluation root kind: " & Node.Kind_Name;
      end case;

      if Expected_Kind = Kind_Bool then
         Result := Get_Truthy (Local_Context, Node.As_Lkql_Node, Result);
      elsif Expected_Kind in Valid_Primitive_Kind then
         Check_Kind (Local_Context, Node.As_Lkql_Node, Expected_Kind, Result);
      end if;

      if Local_Context /= Ctx then
         Local_Context.Release_Current_Frame;
      end if;

      return Result;

   exception
      when Stop_Evaluation_Error =>
         pragma Assert
           (Is_Error (Ctx.Last_Error),
            "Stop Evaluation Error raised without adding the "
            & "error to the evaluation context");

         if Ctx.Last_Error.AST_Node.Is_Null then
            Ctx.Attach_Node_To_Last_Error (Node.As_Lkql_Node);
         end if;

         if Local_Context /= Ctx then
            Local_Context.Release_Current_Frame;
         end if;

         raise;
      when others =>
         if Local_Context /= Ctx then
            Local_Context.Release_Current_Frame;
         end if;

         raise;
   end Eval;

   ---------------
   -- Eval_List --
   ---------------

   function Eval_List
     (Ctx : Eval_Context; Node : L.Lkql_Node_List) return Primitive
   is
      Result : Primitive;
   begin
      if Node.Children'Length = 0 then
         return Make_Unit_Primitive;
      end if;

      for Child of Node.Children loop
         begin
            Result := Eval (Ctx, Child);
         end;
      end loop;

      return Result;
   end Eval_List;

   -------------------
   -- Eval_Val_Decl --
   -------------------

   function Eval_Val_Decl
     (Ctx : Eval_Context; Node : L.Val_Decl) return Primitive
   is
      Identifier : constant Symbol_Type := Symbol (Node.F_Identifier);
   begin
      if Ctx.Exists_In_Local_Env (Identifier) then
         Raise_Already_Existing_Symbol (Ctx,
                                        Identifier,
                                        Node.F_Identifier.As_Lkql_Node);
      end if;

      Ctx.Add_Binding (Identifier, Eval (Ctx, Node.F_Value));
      return Make_Unit_Primitive;
   end Eval_Val_Decl;

   -------------------
   -- Eval_Fun_Decl --
   -------------------

   function Eval_Fun_Decl
     (Ctx : Eval_Context; Node : L.Fun_Decl) return Primitive
   is
      Identifier : constant Symbol_Type := Symbol (Node.F_Name);
   begin
      if Ctx.Exists_In_Local_Env (Identifier) then
         Raise_Already_Existing_Symbol (Ctx,
                                        Identifier,
                                        Node.F_Name.As_Lkql_Node);
      end if;

      Ctx.Add_Binding
        (Identifier,
         Eval_Fun_Expr
           (Ctx,
            Node.F_Fun_Expr.As_Base_Function,
            With_Call_Cache =>
              not Node.F_Annotation.Is_Null
              and then Node.F_Annotation.F_Name.P_Sym = "memoized"));

      return Make_Unit_Primitive;

   end Eval_Fun_Decl;

   ------------------------
   -- Eval_Selector_Decl --
   ------------------------

   function Eval_Selector_Decl
     (Ctx : Eval_Context; Node : L.Selector_Decl) return Primitive
   is
      Identifier : constant Symbol_Type := Symbol (Node.F_Name);
   begin
      if Ctx.Exists_In_Local_Env (Identifier) then
         Raise_Already_Existing_Symbol (Ctx,
                                        Identifier,
                                        Node.F_Name.As_Lkql_Node);
      end if;

      --  The selector declaration will hold a reference to its original env,
      --  and can reference vars coming from it, so hold a reference on it.
      LKQL.Eval_Contexts.Inc_Ref (Ctx.Frames);

      Ctx.Add_Binding
        (Identifier,
         Make_Selector
           (Node, Primitives.Environment_Access (Ctx.Frames), Ctx.Pool,
            With_Call_Cache =>
              not Node.F_Annotation.Is_Null
              and then Node.F_Annotation.F_Name.P_Sym = "memoized"));

      return Make_Unit_Primitive;
   end Eval_Selector_Decl;

   -------------------
   -- Eval_Fun_Expr --
   -------------------

   function Eval_Fun_Expr
     (Ctx             : Eval_Context;
      Node            : L.Base_Function;
      With_Call_Cache : Boolean := False) return Primitive is
   begin
      --  The function will hold a reference to its original env, and can
      --  reference vars coming from it, so hold a reference on it.
      LKQL.Eval_Contexts.Inc_Ref (Ctx.Frames);

      return Make_Function
        (Node,
         Primitives.Environment_Access (Ctx.Frames),
         Ctx.Pool,
         With_Call_Cache => With_Call_Cache);
   end Eval_Fun_Expr;

   ---------------------
   -- Eval_identifier --
   ---------------------

   function Eval_Identifier
     (Ctx : Eval_Context; Node : L.Identifier) return Primitive
   is
      use String_Value_Maps;
      Position : constant Cursor := Ctx.Lookup (Symbol (Node));
   begin
      if Has_Element (Position) then
         return Element (Position);
      end if;

      Raise_Unknown_Symbol (Ctx, Node);
   end Eval_Identifier;

   ----------------
   -- Eval_Tuple --
   ----------------

   function Eval_Tuple (Ctx : Eval_Context; Node : L.Tuple) return Primitive is
      Ret : constant Primitive := Make_Empty_Tuple (Ctx.Pool);
   begin
      for Sub_Expr of Node.F_Exprs loop
         Ret.List_Val.Elements.Append (Eval (Ctx, Sub_Expr));
      end loop;

      return Ret;
   end Eval_Tuple;

   -------------------------
   -- Eval_Bool_Literal --
   -------------------------

   function Eval_Bool_Literal (Node : L.Bool_Literal) return Primitive is
      use type LCO.Lkql_Node_Kind_Type;
      Value : constant Boolean := (Node.Kind = LCO.Lkql_Bool_Literal_True);
   begin
      return To_Primitive (Value);
   end Eval_Bool_Literal;

   -----------------------
   -- Eval_Unit_Literal --
   -----------------------

   function Eval_Unit_Literal (Node : L.Unit_Literal) return Primitive is
     (Make_Unit_Primitive);

   -----------------------
   -- Eval_If_Then_Else --
   -----------------------

   function Eval_If_Then_Else
     (Ctx : Eval_Context; Node : L.If_Then_Else) return Primitive
   is
      Cond : constant Primitive :=
        Eval (Ctx, Node.F_Condition, Expected_Kind => Kind_Bool);
   begin
      return (if Bool_Val (Cond)
              then Eval (Ctx, Node.F_Then_Expr)
              else Eval (Ctx, Node.F_Else_Expr));
   end Eval_If_Then_Else;

   -----------------
   -- Eval_Bin_Op --
   -----------------

   function Eval_Bin_Op (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive
   is
   begin
      return (case Node.F_Op.Kind is
                 when LCO.Lkql_Op_And
                    | LCO.Lkql_Op_Or
                 =>
                    Eval_Short_Circuit_Op (Ctx, Node),
                 when others =>
                    Eval_Non_Short_Circuit_Op (Ctx, Node));
   end Eval_Bin_Op;

   function Eval_Un_Op (Ctx : Eval_Context; Node : L.Un_Op) return Primitive is
   begin
      case Node.F_Op.Kind is
      when LCO.Lkql_Op_Plus =>
         return Eval (Ctx, Node.F_Operand, Kind_Int);
      when LCO.Lkql_Op_Minus =>
         return To_Primitive
           (-Int_Val (Eval (Ctx, Node.F_Operand, Kind_Int)), Ctx.Pool);
      when LCO.Lkql_Op_Not =>
         return To_Primitive
           (not Bool_Val (Eval (Ctx, Node.F_Operand, Kind_Bool)));
      when others =>
         raise Assertion_Error;
      end case;
   end Eval_Un_Op;

   -------------------------------
   -- Eval_Non_Short_Circuit_Op --
   -------------------------------

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive
   is
      Left   : constant Primitive := Eval (Ctx, Node.F_Left);
      Right  : constant Primitive := Eval (Ctx, Node.F_Right);
   begin
      case Node.F_Op.Kind is

      when LCO.Lkql_Op_Plus   =>
         Check_Kind (Ctx, Node.F_Left.As_Lkql_Node, Kind_Int, Left);
         Check_Kind (Ctx, Node.F_Right.As_Lkql_Node, Kind_Int, Right);
         return To_Primitive
           (Int_Val (Left) + Int_Val (Right), Ctx.Pool);

      when LCO.Lkql_Op_Minus  =>
         Check_Kind (Ctx, Node.F_Left.As_Lkql_Node, Kind_Int, Left);
         Check_Kind (Ctx, Node.F_Right.As_Lkql_Node, Kind_Int, Right);
         return To_Primitive
           (Int_Val (Left) - Int_Val (Right), Ctx.Pool);

      when LCO.Lkql_Op_Mul    =>
         Check_Kind (Ctx, Node.F_Left.As_Lkql_Node, Kind_Int, Left);
         Check_Kind (Ctx, Node.F_Right.As_Lkql_Node, Kind_Int, Right);
         return To_Primitive
           (Int_Val (Left) * Int_Val (Right), Ctx.Pool);

      when LCO.Lkql_Op_Div    =>
         Check_Kind (Ctx, Node.F_Left.As_Lkql_Node, Kind_Int, Left);
         Check_Kind (Ctx, Node.F_Right.As_Lkql_Node, Kind_Int, Right);
         if Int_Val (Right) = Zero then
            raise Unsupported_Error with "Zero division";
         end if;
         return To_Primitive
           (Int_Val (Left) / Int_Val (Right), Ctx.Pool);

      when LCO.Lkql_Op_Eq     =>
         return Equals (Left, Right);
      when LCO.Lkql_Op_Neq    =>
         return To_Primitive (not Bool_Val (Equals (Left, Right)));

      when LCO.Lkql_Op_Concat => return Concat (Left, Right, Ctx.Pool);

      when LCO.Lkql_Op_Lt     => return Lt (Left, Right);
      when LCO.Lkql_Op_Leq    => return Lte (Left, Right);
      when LCO.Lkql_Op_Gt     => return Gt (Left, Right);
      when LCO.Lkql_Op_Geq    => return Gte (Left, Right);
      when others =>
         raise Assertion_Error with
           "Not a non-short-cirtcuit operator kind: " &
              Node.F_Op.Kind_Name;
      end case;
   exception
      when E : Unsupported_Error =>
         Raise_From_Exception (Ctx, E, Node);
   end Eval_Non_Short_Circuit_Op;

   ---------------------------
   -- Eval_Short_Circuit_Op --
   ---------------------------

   function Eval_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive
   is
      Result  : Boolean;
      Left    : constant L.Lkql_Node := Node.F_Left.As_Lkql_Node;
      Right   : constant L.Lkql_Node := Node.F_Right.As_Lkql_Node;

      Left_Result  : constant Boolean
        := Bool_Val (Eval (Ctx, Left, Expected_Kind => Kind_Bool));
   begin

      --  We eval the result of the right side expression inline to keep the
      --  operators short circuit.

      Result :=
        (case Node.F_Op.Kind is
            when LCO.Lkql_Op_And =>
              Left_Result and then Bool_Val (Eval
                (Ctx, Right, Expected_Kind => Kind_Bool)),
            when LCO.Lkql_Op_Or  =>
              Left_Result or else Bool_Val (Eval
                (Ctx, Right, Expected_Kind => Kind_Bool)),
            when others          =>
               raise Assertion_Error
                 with "Not a short-circuit operator kind: "
         & Node.F_Op.Kind_Name);
      return To_Primitive (Result);
   end Eval_Short_Circuit_Op;

   --------------------
   -- Eval_Dot_Acess --
   --------------------

   function Eval_Dot_Access
     (Ctx : Eval_Context; Node : L.Dot_Access) return Primitive
   is
      Receiver    : constant Primitive := Eval (Ctx, Node.F_Receiver);
      Member_Name : constant Text_Type := Node.F_Member.Text;
   begin
      declare
         Builtin_Desc : constant Builtin_Method_Descriptor :=
           (Receiver.Kind,
            Symbol (Node.F_Member));

         Cur          : constant Builtin_Methods_Maps.Cursor :=
           Get_Builtin_Methods (Ctx.Kernel).Find (Builtin_Desc);
         use Builtin_Methods_Maps;
      begin
         --  Since this is a propertylike call to a builtin function, we filter
         --  builtin functions that have more than one argument.
         if Has_Element (Cur) and then Element (Cur).N = 1 then
            return Element (Cur).Fn_Access
              (Ctx, (1 => Receiver));
         end if;
      end;

      case Kind (Receiver) is
         when Kind_Object =>
            declare
               R : constant Primitive_Maps.Cursor :=
                 Receiver.Obj_Assocs.Elements.Find
                   (Symbol (Node.F_Member));
            begin
               if Primitive_Maps.Has_Element (R) then
                  return Primitive_Maps.Element (R);
               else
                  Raise_And_Record_Error
                    (Ctx, Make_Eval_Error (Node, "No such member"));
               end if;
            end;
         when Kind_Node =>
            if Is_Nullish (Receiver) then
                  Raise_And_Record_Error
                    (Ctx, Make_Eval_Error
                       (Node, "Null receiver in dot access"));
            end if;

            return Node_Data.Access_Node_Field
              (Ctx, Node_Val (Receiver), Node.F_Member);

         when Kind_Namespace =>

            declare
               R : constant String_Value_Maps.Cursor :=
                 Lookup
                   (Eval_Contexts.Environment_Access
                      (Receiver.Namespace).all,
                    Symbol (Node.F_Member),
                    Local => True);
            begin
               if String_Value_Maps.Has_Element (R) then
                  return String_Value_Maps.Element (R);
               else
                  Raise_And_Record_Error
                    (Ctx, Make_Eval_Error (Node, "No such member"));
               end if;
            end;

         when others =>
            return Primitives.Data (Receiver, Member_Name, Ctx.Pool);
      end case;
   exception
      when Unsupported_Error =>
         Raise_Invalid_Member (Ctx, Node, Receiver);
   end Eval_Dot_Access;

   ----------------------
   -- Eval_Safe_Access --
   ----------------------

   function Eval_Safe_Access
     (Ctx  : Eval_Context; Node : L.Safe_Access) return Primitive
   is
      Receiver : constant LK.Lk_Node :=
        Node_Val (Eval (Ctx, Node.F_Receiver, Expected_Kind => Kind_Node));
   begin
      return (if Receiver.Is_Null
              then To_Primitive (Receiver, Ctx.Pool)
              else Node_Data.Access_Node_Field
                (Ctx, Receiver, Node.F_Member));
   end Eval_Safe_Access;

   -------------
   -- Eval Is --
   -------------

   function Eval_Is
     (Ctx : Eval_Context; Node : L.Is_Clause) return Primitive
   is
      Local_Ctx     : Eval_Context := Ctx.Create_New_Frame;
      Tested_Node   : constant Primitive :=
        Eval (Local_Ctx, Node.F_Node_Expr, Kind_Node);
      Success       : constant Boolean :=
        Match_Pattern (Local_Ctx, Node.F_Pattern, Tested_Node).Is_Success;
   begin
      Local_Ctx.Release_Current_Frame;
      return To_Primitive (Success);
   end Eval_Is;

   -------------
   -- Eval_In --
   -------------

   function Eval_In
     (Ctx : Eval_Context; Node : L.In_Clause) return Primitive
   is
      Tested_Value : constant Primitive := Eval (Ctx, Node.F_Value_Expr);
      Tested_List  : constant Primitive :=
        Eval (Ctx, Node.F_List_Expr, Kind_List);
   begin
      return To_Primitive (Contains (Tested_List, Tested_Value));
   end Eval_In;

   ----------------
   -- Eval_Query --
   ----------------

   function Eval_Query
     (Ctx : Eval_Context; Node : L.Query) return Primitive
   is
      Local_Ctx    : Eval_Context := Ctx.Create_New_Frame;
      Current_Node : LK.Lk_Node;
      Iter         : Lk_Node_Iterator'Class :=
         Make_Query_Iterator (Local_Ctx, Node);
      Result       : Primitive;

      use LCO;
   begin
      if Node.F_Query_Kind.Kind = Lkql_Query_Kind_First then
         if Iter.Next (Current_Node) then
            Result := To_Primitive (Current_Node, Local_Ctx.Pool);
         else
            Result := To_Primitive (LK.No_Lk_Node, Local_Ctx.Pool);
         end if;
      else
         Result := Make_Empty_List (Local_Ctx.Pool);

         while Iter.Next (Current_Node) loop
            Append (Result, To_Primitive (Current_Node, Local_Ctx.Pool));
         end loop;
      end if;

      Iter.Release;
      Local_Ctx.Release_Current_Frame;
      return Result;
   end Eval_Query;

   -------------------
   -- Eval_Indexing --
   -------------------

   function Eval_Indexing
     (Ctx : Eval_Context; Node : L.Indexing) return Primitive
   is
      List  : constant Primitive := Eval (Ctx, Node.F_Collection_Expr);

      use LCO;
      Raise_If_OOB : constant Boolean :=
        L.Kind (Node) /= LCO.Lkql_Safe_Indexing;
   begin
      if Kind (List) not in Kind_List | Kind_Tuple | Kind_Node | Kind_Iterator
      then
         Raise_Invalid_Type
           (Ctx, Node.As_Lkql_Node, "list, tuple, node or iterator", List);
      end if;

      declare
         Index : constant Integer :=
           +Int_Val (Eval (Ctx, Node.F_Index_Expr, Kind_Int));
      begin
         case Kind (List) is
         when Kind_Node =>
            return To_Primitive
               (List.Node_Val.Child (Index),
                Ctx.Pool);

         when Kind_Iterator =>
            declare
               Nb_Values_To_Consume : constant Natural := Integer'Max
                 (Index - List.Iter_Cache.Elements.Last_Index, 0);
            begin
               Consume (List, Nb_Values_To_Consume);
               return Get (List.Iter_Cache, Index, Raise_If_OOB);
            end;

         when others =>
            return Get (List, Index, Raise_If_OOB);
         end case;
      end;
   exception
      when E : Unsupported_Error =>
         Raise_From_Exception (Ctx, E, Node);
   end Eval_Indexing;

   -----------------------------
   -- Eval_List_Comprehension --
   -----------------------------

   function Eval_List_Comprehension
     (Ctx : Eval_Context; Node : L.List_Comprehension) return Primitive
   is
      Comprehension_Envs    : constant Comprehension_Env_Iter :=
        Make_Comprehension_Environment_Iter (Ctx, Node.F_Generators);
      Guard_Filter          : constant Comprehension_Guard_Filter :=
        Make_Guard_Filter (Ctx, Node.F_Guard);
      Comprehension_Closure : constant Closure :=
        Make_Closure (Ctx, Node.F_Expr);
      Comprehension_Values  : Env_Primitive_Maps.Map_Iter :=
        (if Node.F_Guard.Is_Null
         then
            Env_Primitive_Maps.Map (Comprehension_Envs, Comprehension_Closure)
         else
            Env_Primitive_Maps.Map
              (Environment_Iters.Filter (Comprehension_Envs, Guard_Filter),
               Comprehension_Closure));
      Result : constant Primitive := To_Primitive
        (Comprehension_Values, Ctx.Pool);
   begin
      Comprehension_Values.Release;
      return Result;
   end Eval_List_Comprehension;

   function Environment_Iter_For_Assoc
     (Ctx    : Eval_Context;
      Assoc  : L.List_Comp_Assoc;
      Nested : Comprehension_Env_Iter_Access)
      return Comprehension_Env_Iter_Access;

   ---------------------
   -- Eval_Block_Expr --
   ---------------------

   function Eval_Block_Expr
     (Ctx : Eval_Context; Node : L.Block_Expr) return Primitive
   is
      Local_Ctx : Eval_Context;
      Dummy     : Primitive;
   begin
      --  Create a frame for the block
      Local_Ctx := Ctx.Create_New_Frame;

      --  Add Val_Decl bindings to the newly created frame
      for Body_Step of Node.F_Body loop
         declare
            Dummy : Primitive := Eval (Local_Ctx, Body_Step);
         begin
            null;
         end;
      end loop;

      --  Eval the expression in the context of the new frame, release the
      --  frame, return.
      return Ret : constant Primitive := Eval (Local_Ctx, Node.F_Expr) do
         Local_Ctx.Release_Current_Frame;
      end return;
   end Eval_Block_Expr;

   ----------------------
   -- Eval_Body_Decl --
   ----------------------

   function Eval_Body_Decl
     (Ctx : Eval_Context; Node : L.Block_Body_Decl) return Primitive is
   begin
      return Eval (Ctx, Node.F_Decl);
   end Eval_Body_Decl;

   ----------------------
   -- Eval_Body_Expr --
   ----------------------

   function Eval_Body_Expr
     (Ctx : Eval_Context; Node : L.Block_Body_Expr) return Primitive
   is
      Ret : constant Primitive := Eval (Ctx, Node.F_Expr);
   begin
      if Is_Nullish (Ret) then
         return Ret;
      end if;

      Raise_And_Record_Error
        (Ctx, Make_Eval_Error
          (Node, "Can't ignore the return value of an expr in a block expr"));
   end Eval_Body_Expr;

   ----------------
   -- Eval_Match --
   ----------------

   function Eval_Match (Ctx : Eval_Context; Node : L.Match) return Primitive is
      use Primitive_Options;
      Result        : Primitive;
      Local_Context : Eval_Context;
      Matched_Value : constant Primitive := Eval (Ctx, Node.F_Matched_Val);

   begin

      Local_Context := Ctx.Create_New_Frame;

      declare
         Match_Data    : constant Match_Array_Result :=
           Match_Pattern_Array (Ctx, Node.P_Patterns, Matched_Value);
      begin

         if Match_Data.Index = Match_Index'First then
            Local_Context.Release_Current_Frame;
            return Make_Unit_Primitive;
         end if;

         Local_Context.Add_Binding
           ("this", Extract (Match_Data.Matched_Value));

         Result :=
           Eval (Local_Context, Node.P_Nth_Expression (Match_Data.Index));

         Local_Context.Release_Current_Frame;

         return Result;
      end;
   end Eval_Match;

   -----------------
   -- Eval_Import --
   -----------------

   function Eval_Import
     (Ctx : Eval_Context; Node : L.Import) return Primitive
   is
      Package_Name : constant String := Image (Node.F_Name.Text);
      Unit         : constant L.Analysis_Unit :=
        Ctx.Get_Lkql_Unit (Package_Name, From => Node.Unit);
      Frame        : constant Eval_Context := Ctx.Create_New_Frame;
      Dummy        : constant Primitive := Eval (Frame, Unit.Root);
      NS           : constant Primitive :=
        Make_Namespace
          (Primitives.Environment_Access (Frame.Frames), Unit.Root, Ctx.Pool);
   begin
      Ctx.Add_Binding (Symbol (Node.F_Name), NS);
      return Make_Unit_Primitive;
   end Eval_Import;

   -----------------------
   -- Eval_List_Literal --
   -----------------------

   function Eval_List_Literal
     (Ctx : Eval_Context; Node : L.List_Literal) return Primitive
   is
      Res : constant Primitive := Make_Empty_List (Ctx.Pool);
   begin
      for Expr of Node.F_Exprs loop
         Res.List_Val.Elements.Append (Eval (Ctx, Expr));
      end loop;
      return Res;
   end Eval_List_Literal;

   -------------------------
   -- Eval_Object_Literal --
   -------------------------

   function Eval_Object_Literal
     (Ctx : Eval_Context; Node : L.Object_Literal) return Primitive
   is
      Res : constant Primitive := Make_Empty_Object (Ctx.Pool);
   begin
      for Assoc of Node.F_Assocs loop
         Res.Obj_Assocs.Elements.Include
           (Symbol (Assoc.F_Name), Eval (Ctx, Assoc.F_Expr));
      end loop;
      return Res;
   end Eval_Object_Literal;

   ----------------------------
   -- Eval_At_Object_Literal --
   ----------------------------

   function Eval_At_Object_Literal
     (Ctx : Eval_Context; Node : L.At_Object_Literal) return Primitive
   is
      Res   : constant Primitive := Make_Empty_Object (Ctx.Pool);
   begin
      for Assoc of Node.F_Assocs loop
         Res.Obj_Assocs.Elements.Include
           (Symbol (Assoc.F_Name),
            (if Assoc.F_Expr.Is_Null
             then Make_Empty_List (Ctx.Pool)
             else Eval (Ctx, Assoc.F_Expr)));
      end loop;
      return Res;
   end Eval_At_Object_Literal;

   -----------------
   -- Eval_Unwrap --
   -----------------

   function Eval_Unwrap (Ctx : Eval_Context; Node : L.Unwrap) return Primitive
   is
      Value : constant LK.Lk_Node :=
        Node_Val (Eval (Ctx, Node.F_Node_Expr, Expected_Kind => Kind_Node));
   begin
      return To_Primitive (Value, Ctx.Pool);
   end Eval_Unwrap;

   -----------------------------------------
   -- Make_Comprehension_Environment_Iter --
   -----------------------------------------

   function Make_Comprehension_Environment_Iter
     (Ctx : Eval_Context; Node : L.List_Comp_Assoc_List)
      return Comprehension_Env_Iter
   is
      Current_Env : Comprehension_Env_Iter_Access := null;
      Res       : Comprehension_Env_Iter;
   begin
      for I in reverse Node.Children'Range loop
         declare
            Current_Assoc   : constant L.List_Comp_Assoc :=
              Node.Children (I).As_List_Comp_Assoc;
         begin
            Current_Env :=
              Environment_Iter_For_Assoc (Ctx, Current_Assoc, Current_Env);
         end;
      end loop;

      Res := Current_Env.all;
      Environment_Iters.Free_Iterator
        (Environment_Iters.Iterator_Access (Current_Env));
      return Res;
   end Make_Comprehension_Environment_Iter;

   --------------------------------
   -- Environment_Iter_For_Assoc --
   --------------------------------

   function Environment_Iter_For_Assoc
     (Ctx    : Eval_Context;
      Assoc  : L.List_Comp_Assoc;
      Nested : Comprehension_Env_Iter_Access)
      return Comprehension_Env_Iter_Access
   is
      use Primitive_Options;
      Generator_Value  : constant Primitive :=
        Eval (Ctx, Assoc.F_Coll_Expr);
      Generator_Iter   : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'(To_Iterator (Generator_Value, Ctx.Pool));
      Binding_Name     : constant Symbol_Type :=
        Symbol (Assoc.F_Binding_Name);
      Nested_Resetable : constant Environment_Iters.Resetable_Access :=
        (if Nested = null then null
         else new Environment_Iters.Resetable_Iter'
           (Environment_Iters.Resetable
                (Environment_Iters.Iterator_Access (Nested))));
      Current_Element : Primitive_Options.Option;
      First_Element   : Primitive;
   begin
      if Generator_Iter.Next (First_Element) then
         Current_Element := To_Option (First_Element);
      end if;

      return new Comprehension_Env_Iter'
        (Binding_Name, Current_Element, Generator_Iter, Nested_Resetable);
   end Environment_Iter_For_Assoc;

   function Update_Nested_Env (Iter   : in out Comprehension_Env_Iter;
                               Result : out Environment_Map) return Boolean;
   --  Return a new enviroment built by adding the current iterator's binding
   --  to the environment produced by it's 'Nested' iterator.

   function Create_New_Env (Iter   : in out Comprehension_Env_Iter;
                            Result : out Environment_Map) return Boolean;
   --  Return a new environment containing only the current iterator's binding

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Comprehension_Env_Iter;
                             Result : out Environment_Map) return Boolean
   is
      use type Environment_Iters.Resetable_Access;
   begin
      if Iter.Nested /= null then
         return Update_Nested_Env (Iter, Result);
      else
         return Create_New_Env (Iter, Result);
      end if;
   end Next;

   procedure Update_Current_Element (Iter : in out Comprehension_Env_Iter);

   -----------------------
   -- Update_Nested_Env --
   -----------------------

   function Update_Nested_Env (Iter   : in out Comprehension_Env_Iter;
                               Result : out Environment_Map) return Boolean
   is
      use Primitive_Options;
      Env            : Environment_Map;
      Nested_Exists  : Boolean;
   begin
      if Is_None (Iter.Current_Element) then
         return False;
      end if;

      Nested_Exists := Iter.Nested.Next (Env);

      if not Nested_Exists then
         Update_Current_Element (Iter);
         Iter.Nested.Reset;
         --  Stop the iteation if we can't build a complete environment
         --  after updating the current element and reseting the nested
         --  iterator.
         if Is_None (Iter.Current_Element) or else
           not Iter.Nested.Next (Env)
         then
            return False;
         end if;
      end if;

      Env.Include (Iter.Binding_Name, Extract (Iter.Current_Element));
      Result := Env;
      return True;
   end Update_Nested_Env;

   ----------------------------
   -- Update_Current_Element --
   ----------------------------

   procedure Update_Current_Element (Iter : in out Comprehension_Env_Iter) is
      use Primitive_Options;
      Element        : Primitive;
      Element_Exists : constant Boolean := Iter.Gen.Next (Element);
   begin
      if Element_Exists then
         Iter.Current_Element := To_Option (Element);
      else
         Iter.Current_Element := None;
      end if;
   end Update_Current_Element;

   --------------------
   -- Create_New_Env --
   --------------------

   function Create_New_Env (Iter   : in out Comprehension_Env_Iter;
                            Result : out Environment_Map) return Boolean
   is
      use Primitive_Options;
   begin
      if Is_None (Iter.Current_Element) then
         return False;
      end if;

      Result.Include (Iter.Binding_Name, Extract (Iter.Current_Element));
      Update_Current_Element (Iter);
      return True;
   end Create_New_Env;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Iter : Comprehension_Env_Iter) return Comprehension_Env_Iter
   is
      use type Environment_Iters.Resetable_Access;
      Gen_Copy    : constant Primitive_Iters.Iterator_Access :=
        new Primitive_Iters.Iterator_Interface'Class'(
          Primitive_Iters.Iterator_Interface'Class (Iter.Gen.Clone));
      Nested_Copy : constant Environment_Iters.Resetable_Access :=
        (if Iter.Nested = null then null
         else new Environment_Iters.Resetable_Iter'(Iter.Nested.Clone));
   begin
      return (Iter.Binding_Name, Iter.Current_Element, Gen_Copy, Nested_Copy);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Comprehension_Env_Iter) is
   begin
      Primitive_Iters.Release_Access (Iter.Gen);
      Environment_Iters.Release_Access
        (Environment_Iters.Iterator_Access (Iter.Nested));
   end Release;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate (Self    : in out Closure;
                                 Element : Environment_Map) return Primitive
   is
   begin
      return Eval (Self.Ctx, Self.Body_Expr, Local_Bindings => Element);
   end Evaluate;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Self : Closure) return Closure is
   begin
      return Make_Closure (Self.Ctx, Self.Body_Expr);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Closure) is
   begin
      Self.Ctx.Release_Current_Frame;
   end Release;

   ------------------
   -- Make_Closure --
   ------------------

   function Make_Closure
     (Ctx : Eval_Context; Body_Expr : L.Expr) return Closure
   is
   begin
      return Closure'(Ctx.Ref_Frame, Body_Expr);
   end Make_Closure;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Self : in out Comprehension_Guard_Filter;
                      Element : Environment_Map) return Boolean
   is
      Result : constant Primitive :=
        Eval (Self.Ctx, Self.Guard, Kind_Bool, Element);
   begin
      return Bool_Val (Result);
   end Evaluate;

   -----------
   -- Clone --
   -----------

   function Clone
     (Self : Comprehension_Guard_Filter) return Comprehension_Guard_Filter
   is
   begin
      return Self;
   end Clone;

   -----------------------
   -- Make_Guard_Filter --
   -----------------------

   function Make_Guard_Filter (Ctx : Eval_Context;
                               Guard : L.Expr)
                               return Comprehension_Guard_Filter
   is
   begin
      return Comprehension_Guard_Filter'(Ctx, Guard);
   end Make_Guard_Filter;

end LKQL.Evaluation;
