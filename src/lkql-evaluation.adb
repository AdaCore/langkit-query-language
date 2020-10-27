with LKQL.Checks;         use LKQL.Checks;
with LKQL.Errors;         use LKQL.Errors;
with LKQL.Queries;        use LKQL.Queries;
with LKQL.Patterns;       use LKQL.Patterns;
with LKQL.Functions;      use LKQL.Functions;
with LKQL.AST_Nodes;      use LKQL.AST_Nodes;
with LKQL.Node_Data;
with LKQL.Depth_Nodes;    use LKQL.Depth_Nodes;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with LKQL.Error_Handling; use LKQL.Error_Handling;

with Ada.Assertions;                  use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body LKQL.Evaluation is

   function Eval_List
     (Ctx : Eval_Context; Node : L.LKQL_Node_List) return Primitive;

   function Eval_Val_Decl
     (Ctx : Eval_Context; Node : L.Val_Decl) return Primitive;

   function Eval_Identifier
     (Ctx : Eval_Context; Node : L.Identifier) return Primitive;

   function Eval_Integer_Literal (Node : L.Integer_Literal) return Primitive;

   function Eval_String_Literal (Node : L.String_Literal) return Primitive;

   function Eval_Bool_Literal (Node : L.Bool_Literal) return Primitive;

   function Eval_Unit_Literal (Node : L.Unit_Literal) return Primitive;

   function Eval_If_Then_Else
     (Ctx : Eval_Context; Node : L.If_Then_Else) return Primitive;

   function Eval_Not (Ctx : Eval_Context; Node : L.Not_Node) return Primitive;

   function Eval_Bin_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Dot_Access
     (Ctx : Eval_Context; Node : L.Dot_Access) return Primitive;

   function Eval_Safe_Access
     (Ctx  : Eval_Context; Node : L.Safe_Access) return Primitive;

   function Eval_Dot_Call
     (Ctx : Eval_Context; Node : L.Dot_Call) return Primitive;

   function Eval_Safe_Call
     (Ctx : Eval_Context; Node : L.Safe_Call) return Primitive;

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

   function Eval_Match (Ctx : Eval_Context; Node : L.Match) return Primitive;

   function Eval_Unwrap (Ctx : Eval_Context; Node : L.Unwrap) return Primitive;

   function Make_Comprehension_Environment_Iter
     (Ctx : Eval_Context; Node : L.Arrow_Assoc_List)
      return Comprehension_Env_Iter;
   --  Given a List of Arrow_Assoc, return an iterator that yields the
   --  environments produced by this list of Arrow_Assoc in the context of a
   --  list comprehension.

   procedure Check_Kind (Ctx           : Eval_Context;
                         Node          : L.LKQL_Node;
                         Expected_Kind : Valid_Primitive_Kind;
                         Value         : Primitive);
   --  Raise an exception and register an error in the evaluation context if
   --  `Value` doesn't have the expected kind.

   function Bool_Eval
     (Ctx : Eval_Context; Node : L.LKQL_Node) return Boolean;
   --  Evalaluate the given node and convert to result to an Ada Boolean.
   --  Raise an exception if the result of the node's evaluation is not a
   --  boolean.

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind (Ctx           : Eval_Context;
                         Node          : L.LKQL_Node;
                         Expected_Kind : Valid_Primitive_Kind;
                         Value         : Primitive)
   is
   begin
      if Kind (Value) /= Expected_Kind then
         Raise_Invalid_Kind (Ctx, Node, Expected_Kind, Value);
      end if;
   end Check_Kind;

   ---------------
   -- Bool_Eval --
   ---------------

   function Bool_Eval
     (Ctx : Eval_Context; Node : L.LKQL_Node) return Boolean
   is
      Result : constant Primitive := Eval (Ctx, Node, Kind_Bool);
   begin
      return Bool_Val (Result);
   end Bool_Eval;

   --------------------
   -- Check_And_Eval --
   --------------------

   function Check_And_Eval
     (Ctx  : Eval_Context; Node : L.LKQL_Node'Class) return Primitive
   is
   begin
      Check (Ctx, Node);
      return Eval (Ctx, Node);
   end Check_And_Eval;

   ----------
   -- Eval --
   ----------

   function Eval (Ctx            : Eval_Context;
                  Node           : L.LKQL_Node'Class;
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
         when LCO.LKQL_LKQL_Node_List =>
            Result := Eval_List (Local_Context, Node.As_LKQL_Node_List);
         when LCO.LKQL_Val_Decl =>
            Result := Eval_Val_Decl (Local_Context, Node.As_Val_Decl);
         when LCO.LKQL_Identifier =>
            Result := Eval_Identifier (Local_Context, Node.As_Identifier);
         when LCO.LKQL_Integer_Literal =>
            Result := Eval_Integer_Literal (Node.As_Integer_Literal);
         when LCO.LKQL_String_Literal =>
            Result := Eval_String_Literal (Node.As_String_Literal);
         when LCO.LKQL_Bool_Literal =>
            Result := Eval_Bool_Literal (Node.As_Bool_Literal);
         when LCO.LKQL_Unit_Literal =>
            Result := Eval_Unit_Literal (Node.As_Unit_Literal);
         when LCO.LKQL_If_Then_Else =>
            Result := Eval_If_Then_Else (Local_Context, Node.As_If_Then_Else);
         when LCO.LKQL_Not_Node =>
            Result := Eval_Not (Local_Context, Node.As_Not_Node);
         when LCO.LKQL_Bin_Op =>
            Result := Eval_Bin_Op (Local_Context, Node.As_Bin_Op);
         when LCO.LKQL_Dot_Access =>
            Result := Eval_Dot_Access (Local_Context, Node.As_Dot_Access);
         when LCO.LKQL_Safe_Access =>
            Result := Eval_Safe_Access (Local_Context, Node.As_Safe_Access);
         when LCO.LKQL_Dot_Call =>
            Result := Eval_Dot_Call (Local_Context, Node.As_Dot_Call);
         when LCO.LKQL_Safe_Call =>
            Result := Eval_Safe_Call (Local_Context, Node.As_Safe_Call);
         when LCO.LKQL_Is_Clause =>
            Result := Eval_Is (Local_Context, Node.As_Is_Clause);
         when LCO.LKQL_In_Clause =>
            Result := Eval_In (Local_Context, Node.As_In_Clause);
         when LCO.LKQL_Query =>
            Result := Eval_Query (Local_Context, Node.As_Query);
         when LCO.LKQL_Indexing =>
            Result := Eval_Indexing (Local_Context, Node.As_Indexing);
         when LCO.LKQL_List_Comprehension =>
            Result := Eval_List_Comprehension
              (Local_Context, Node.As_List_Comprehension);
         when LCO.LKQL_Block_Expr =>
            Result := Eval_Block_Expr (Local_Context, Node.As_Block_Expr);
         when LCO.LKQL_Fun_Decl =>
            Result := Make_Unit_Primitive;
         when LCO.LKQL_Fun_Call =>
            Result := Eval_Fun_Call (Local_Context, Node.As_Fun_Call);
         when LCO.LKQL_Selector_Decl =>
            Result := Make_Unit_Primitive;
         when LCO.LKQL_Match =>
            Result := Eval_Match (Local_Context, Node.As_Match);
         when LCO.LKQL_Unwrap =>
            Result := Eval_Unwrap (Local_Context, Node.As_Unwrap);
         when LCO.LKQL_Null_Literal =>
            Result := To_Primitive (Local_Context.Null_Node);
         when others =>
            raise Assertion_Error
              with "Invalid evaluation root kind: " & Node.Kind_Name;
      end case;

      if Expected_Kind in Valid_Primitive_Kind then
         Check_Kind (Local_Context, Node.As_LKQL_Node, Expected_Kind, Result);
      end if;

      if Local_Context /= Ctx then
         Local_Context.Release_Current_Frame;
      end if;

      return Result;

   exception
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
     (Ctx : Eval_Context; Node : L.LKQL_Node_List) return Primitive
   is
      Result : Primitive;
   begin
      if Node.Children'Length = 0 then
         return Make_Unit_Primitive;
      end if;

      for Child of Node.Children loop
         begin
            Result := Eval (Ctx, Child);
         exception
            when Recoverable_Error => null;
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
      Identifier : constant Text_Type :=
        Node.F_Identifier.Text;
   begin
      if Ctx.Exists_In_Local_Env (Identifier) then
         Raise_Already_Existing_Symbol (Ctx,
                                        To_Unbounded_Text (Identifier),
                                        Node.F_Identifier.As_LKQL_Node);
      end if;

      Ctx.Add_Binding (Identifier, Eval (Ctx, Node.F_Value));
      return Make_Unit_Primitive;
   end Eval_Val_Decl;

   ---------------------
   -- Eval_identifier --
   ---------------------

   function Eval_Identifier
     (Ctx : Eval_Context; Node : L.Identifier) return Primitive
   is
      use String_Value_Maps;
      Position : constant Cursor := Ctx.Lookup (To_Unbounded_Text (Node.Text));
   begin
      if Has_Element (Position) then
         return Element (Position);
      end if;

      Raise_Unknown_Symbol (Ctx, Node);
   end Eval_Identifier;

   --------------------------
   -- Eval_Integer_Literal --
   --------------------------

   function Eval_Integer_Literal (Node : L.Integer_Literal) return Primitive is
   begin
      return To_Primitive (Integer'Wide_Wide_Value (Node.Text));
   end Eval_Integer_Literal;

   -------------------------
   -- Eval_String_Literal --
   -------------------------

   function Eval_String_Literal (Node : L.String_Literal) return Primitive is
      Quoted_Literal : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node.Text);
      Literal : constant Unbounded_Text_Type :=
        Unbounded_Slice (Quoted_Literal, 2, Length (Quoted_Literal) - 1);
   begin
      return To_Primitive (Literal);
   end Eval_String_Literal;

   -------------------------
   -- Eval_Bool_Literal --
   -------------------------

   function Eval_Bool_Literal (Node : L.Bool_Literal) return Primitive is
      use type LCO.LKQL_Node_Kind_Type;
      Value : constant Boolean := (Node.Kind = LCO.LKQL_Bool_Literal_True);
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

   --------------
   -- Eval_Not --
   --------------

   function Eval_Not (Ctx : Eval_Context; Node : L.Not_Node) return Primitive
   is
      Value : constant Primitive :=
        Eval (Ctx, Node.F_Value, Expected_Kind => Kind_Bool);
   begin
      return To_Primitive (not Bool_Val (Value));
   end Eval_Not;

   -----------------
   -- Eval_Bin_Op --
   -----------------

   function Eval_Bin_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive
   is
   begin
      return (case Node.F_Op.Kind is
                 when LCO.LKQL_Op_And
                    | LCO.LKQL_Op_Or
                 =>
                    Eval_Short_Circuit_Op (Ctx, Node),
                 when others =>
                    Eval_Non_Short_Circuit_Op (Ctx, Node));
   end Eval_Bin_Op;

   -------------------------------
   -- Eval_Non_Short_Circuit_Op --
   -------------------------------

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive
   is
      Left   : constant Primitive := Eval (Ctx, Node.F_Left);
      Right  : constant Primitive := Eval (Ctx, Node.F_Right);
   begin
      return (case Node.F_Op.Kind is
              when LCO.LKQL_Op_Plus   => Left + Right,
              when LCO.LKQL_Op_Minus  => Left - Right,
              when LCO.LKQL_Op_Mul    => Left * Right,
              when LCO.LKQL_Op_Div    => Left / Right,
              when LCO.LKQL_Op_Eq     => "=" (Left, Right),
              when LCO.LKQL_Op_Neq    => Left /= Right,
              when LCO.LKQL_Op_Concat => Left & Right,
              when LCO.LKQL_Op_Lt     => Left < Right,
              when LCO.LKQL_Op_Leq    => Left <= Right,
              when LCO.LKQL_Op_Gt     => Left > Right,
              when LCO.LKQL_Op_Geq    => Left >= Right,
              when others =>
                 raise Assertion_Error with
                   "Not a non-short-cirtcuit operator kind: " &
                      Node.F_Op.Kind_Name);
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
      Left    : constant L.LKQL_Node := Node.F_Left.As_LKQL_Node;
      Right   : constant L.LKQL_Node := Node.F_Right.As_LKQL_Node;
   begin
      case Node.F_Op.Kind is
      when LCO.LKQL_Op_And =>
         Result :=
           Bool_Eval (Ctx, Left) and then Bool_Eval (Ctx, Right);
      when LCO.LKQL_Op_Or =>
         Result :=
           Bool_Eval (Ctx, Left) or else Bool_Eval (Ctx, Right);
      when others =>
         raise Assertion_Error
           with "Not a short-circuit operator kind: " & Node.F_Op.Kind_Name;
      end case;

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
      if Kind (Receiver) /= Kind_Node then
         return Primitives.Data (Receiver, Member_Name);
      elsif Is_Nullable (Receiver) then
         Raise_Null_Access (Ctx, Receiver, Node.F_Member);
      else
         return Node_Data.Access_Node_Field
           (Ctx, Node_Val (Receiver), Node.F_Member);
      end if;

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
      Receiver : constant AST_Node_Rc :=
        Node_Val (Eval (Ctx, Node.F_Receiver, Expected_Kind => Kind_Node));
   begin
      return (if Receiver.Get.Is_Null_Node
              then To_Primitive (Receiver, Nullable => True)
              else Node_Data.Access_Node_Field
                (Ctx, Receiver, Node.F_Member));
   end Eval_Safe_Access;

   ---------------------
   -- Eval_Dot_Access --
   ---------------------

   function Eval_Dot_Call
     (Ctx : Eval_Context; Node : L.Dot_Call) return Primitive
   is
      Receiver : constant Primitive := Eval (Ctx, Node.F_Receiver);
   begin
      if Kind (Receiver) /= Kind_Node then
         Raise_Invalid_Kind
           (Ctx, Node.F_Receiver.As_LKQL_Node, Kind_Node, Receiver);
      elsif Is_Nullable (Receiver) then
         Raise_Null_Access (Ctx, Receiver, Node.F_Member);
      end if;

      return Node_Data.Eval_Node_Property
        (Ctx, Node_Val (Receiver), Node.F_Member, Node.F_Arguments);
   end Eval_Dot_Call;

   --------------------
   -- Eval_Safe_Call --
   -------------------

   function Eval_Safe_Call
     (Ctx : Eval_Context; Node : L.Safe_Call) return Primitive
   is
      Receiver : constant AST_Node_Rc :=
        Node_Val (Eval (Ctx, Node.F_Receiver, Expected_Kind => Kind_Node));
   begin
      return (if Receiver.Get.Is_Null_Node
              then To_Primitive (Receiver, Nullable => True)
              else Node_Data.Eval_Node_Property
                (Ctx, Receiver, Node.F_Member, Node.F_Arguments));
   end Eval_Safe_Call;

   -------------
   -- Eval Is --
   -------------

   function Eval_Is
     (Ctx : Eval_Context; Node : L.Is_Clause) return Primitive
   is
      Tested_Node   : constant Primitive :=
        Eval (Ctx, Node.F_Node_Expr, Kind_Node);
      Success       : constant Boolean :=
        Match_Pattern (Ctx, Node.F_Pattern, Tested_Node).Is_Success;
   begin
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
      Current_Node : AST_Node_Rc;
      Iter         : AST_Node_Iterator'Class :=
        Make_Query_Iterator (Ctx, Node);
      Result       : constant Primitive :=  Make_Empty_List;
   begin
      while Iter.Next (Current_Node) loop
         Append (Result, To_Primitive (Current_Node));
      end loop;

      Iter.Release;
      return Result;
   end Eval_Query;

   -------------------
   -- Eval_Indexing --
   -------------------

   function Eval_Indexing
     (Ctx : Eval_Context; Node : L.Indexing) return Primitive
   is
      List  : constant Primitive :=
        Eval (Ctx, Node.F_Collection_Expr, Kind_List);
      Index : constant Primitive :=
        Eval (Ctx, Node.F_Index_Expr, Kind_Int);
   begin
      return Get (List, Int_Val (Index));
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
      Filtered_Envs         : constant Environment_Iters.Filter_Iter :=
        Environment_Iters.Filter (Comprehension_Envs, Guard_Filter);
      Comprehension_Closure : constant Closure :=
        Make_Closure (Ctx, Node.F_Expr);
      Comprehension_Values  : Env_Primitive_Maps.Map_Iter :=
        (if Node.F_Guard.Is_Null
         then
            Env_Primitive_Maps.Map (Comprehension_Envs, Comprehension_Closure)
         else
            Env_Primitive_Maps.Map (Filtered_Envs, Comprehension_Closure));
      Result : constant Primitive := To_Primitive (Comprehension_Values);
   begin
      Comprehension_Values.Release;
      return Result;
   end Eval_List_Comprehension;

   function Environment_Iter_For_Assoc
     (Ctx    : Eval_Context;
      Assoc  : L.Arrow_Assoc;
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
      for V_Decl of Node.F_Vals loop
         Dummy := Eval_Val_Decl (Local_Ctx, V_Decl.As_Val_Decl);
      end loop;

      --  Eval the expression in the context of the new frame, release the
      --  frame, return.
      return Ret : constant Primitive := Eval (Local_Ctx, Node.F_Expr) do
         Local_Ctx.Release_Current_Frame;
      end return;
   end Eval_Block_Expr;

   ----------------
   -- Eval_Match --
   ----------------

   function Eval_Match (Ctx : Eval_Context; Node : L.Match) return Primitive is
      use Primitive_Options;
      Result        : Primitive;
      Local_Context : Eval_Context;
      Matched_Value : constant Primitive := Eval (Ctx, Node.F_Matched_Val);
      Match_Data    : constant Match_Array_Result :=
        Match_Pattern_Array (Ctx, Node.P_Patterns, Matched_Value);
   begin
      if Match_Data.Index = Match_Index'First then
         return Make_Unit_Primitive;
      end if;

      Local_Context := Ctx.Create_New_Frame (Match_Data.Bindings);
      Local_Context.Add_Binding ("it", Extract (Match_Data.Matched_Value));
      Result := Eval (Local_Context, Node.P_Nth_Expression (Match_Data.Index));
      Local_Context.Release_Current_Frame;

      return Result;
   end Eval_Match;

   -----------------
   -- Eval_Unwrap --
   -----------------

   function Eval_Unwrap (Ctx : Eval_Context; Node : L.Unwrap) return Primitive
   is
      Value : constant AST_Node_Rc :=
        Node_Val (Eval (Ctx, Node.F_Node_Expr, Expected_Kind => Kind_Node));
   begin
      return To_Primitive (Value, Nullable => False);
   end Eval_Unwrap;

   -----------------------------------------
   -- Make_Comprehension_Environment_Iter --
   -----------------------------------------

   function Make_Comprehension_Environment_Iter
     (Ctx : Eval_Context; Node : L.Arrow_Assoc_List)
      return Comprehension_Env_Iter
   is
      Current_Env : Comprehension_Env_Iter_Access := null;
      Res       : Comprehension_Env_Iter;
   begin
      for I in reverse Node.Children'Range loop
         declare
            Current_Assoc   : constant L.Arrow_Assoc :=
              Node.Children (I).As_Arrow_Assoc;
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
      Assoc  : L.Arrow_Assoc;
      Nested : Comprehension_Env_Iter_Access)
      return Comprehension_Env_Iter_Access
   is
      use Primitive_Options;
      Generator_Value  : constant Primitive :=
        Eval (Ctx, Assoc.F_Coll_Expr);
      Generator_Iter   : constant Primitive_Iter_Access :=
        new Primitive_Iter'Class'(To_Iterator (Generator_Value));
      Binding_Name     : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Assoc.F_Binding_Name.Text);
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
      return Closure'(Ctx.Clone_Frame, Body_Expr);
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
