with Queries;                    use Queries;
with Node_Data;                  use Node_Data;
with Patterns.Nodes;             use Patterns.Nodes;
with Interpreter.Errors;         use Interpreter.Errors;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Libadalang.Iterators;     use Libadalang.Iterators;
with Libadalang.Common;        use type Libadalang.Common.Ada_Node_Kind_Type;
with Libadalang.Introspection; use Libadalang.Introspection;

with Ada.Assertions;                  use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Interpreter.Evaluation is

   function Eval_List
     (Ctx : Eval_Context; Node : L.Expr_List) return Primitive;

   function Eval_Assign
     (Ctx : Eval_Context; Node : L.Assign) return Primitive;

   function Eval_Identifier
     (Ctx : Eval_Context; Node : L.Identifier) return Primitive;

   function Eval_Integer (Node : L.Integer) return Primitive;

   function Eval_String_Literal (Node : L.String_Literal) return Primitive;

   function Eval_Bool_Literal (Node : L.Bool_Literal) return Primitive;

   function Eval_Print
     (Ctx : Eval_Context; Node : L.Print_Stmt) return Primitive;

   function Eval_Bin_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Short_Circuit_Op
     (Ctx : Eval_Context; Node : L.Bin_Op) return Primitive;

   function Eval_Dot_Access
     (Ctx : Eval_Context; Node : L.Dot_Access) return Primitive;

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

   function Eval_Val_Expr
     (Ctx : Eval_Context; Node : L.Val_Expr) return Primitive;

   function Eval_Fun_Def
     (Ctx : Eval_Context; Node : L.Fun_Def) return Primitive;

   function Eval_Fun_Call
     (Ctx : Eval_Context; Node : L.Fun_Call) return Primitive;

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
      Local_Context : Eval_Context :=
        (if Local_Bindings.Is_Empty then Ctx
         else Ctx.Create_New_Frame (Local_Bindings));
   begin

      Result :=
        (case Node.Kind is
            when LCO.LKQL_Expr_List =>
              Eval_List (Local_Context, Node.As_Expr_List),
            when LCO.LKQL_Assign =>
              Eval_Assign (Local_Context, Node.As_Assign),
            when LCO.LKQL_Identifier =>
              Eval_Identifier (Local_Context, Node.As_Identifier),
            when LCO.LKQL_Integer =>
              Eval_Integer (Node.As_Integer),
            when LCO.LKQL_String_Literal =>
              Eval_String_Literal (Node.As_String_Literal),
            when LCO.LKQL_Bool_Literal =>
              Eval_Bool_Literal (Node.As_Bool_Literal),
            when LCO.LKQL_Print_Stmt =>
              Eval_Print (Local_Context, Node.As_Print_Stmt),
            when LCO.LKQL_Bin_Op =>
              Eval_Bin_Op (Local_Context, Node.As_Bin_Op),
            when LCO.LKQL_Dot_Access =>
              Eval_Dot_Access (Local_Context, Node.As_Dot_Access),
            when LCO.LKQL_Is_Clause =>
              Eval_Is (Local_Context, Node.As_Is_Clause),
            when LCO.LKQL_In_Clause =>
              Eval_In (Local_Context, Node.As_In_Clause),
            when LCO.LKQL_Query =>
              Eval_Query (Local_Context, Node.As_Query),
            when LCO.LKQL_Indexing =>
              Eval_Indexing (Local_Context, Node.As_Indexing),
            when LCO.LKQL_List_Comprehension =>
              Eval_List_Comprehension
                 (Local_Context, Node.As_List_Comprehension),
            when LCO.LKQL_Val_Expr =>
              Eval_Val_Expr (Local_Context, Node.As_Val_Expr),
            when LCO.LKQL_Fun_Def =>
              Eval_Fun_Def (Ctx, Node.As_Fun_Def),
            when LCO.LKQL_Fun_Call =>
              Eval_Fun_Call (Ctx, Node.As_Fun_Call),
            when others =>
               raise Assertion_Error
                 with "Invalid evaluation root kind: " & Node.Kind_Name);

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
     (Ctx : Eval_Context; Node : L.Expr_List) return Primitive
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

   -----------------
   -- Eval_Assign --
   -----------------

   function Eval_Assign
     (Ctx : Eval_Context; Node : L.Assign) return Primitive
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
   end Eval_Assign;

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

   ------------------
   -- Eval_integer --
   ------------------

   function Eval_Integer (Node : L.Integer) return Primitive is
   begin
      return To_Primitive (Integer'Wide_Wide_Value (Node.Text));
   end Eval_Integer;

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

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print
     (Ctx : Eval_Context; Node : L.Print_Stmt) return Primitive
   is
   begin
      Display (Eval (Ctx, Node.F_Value));
      return Make_Unit_Primitive;
   end Eval_Print;

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
              when others =>
                 raise Assertion_Error with
                   "Not a non-short-cirtcuit operator kind: " &
                   Node.F_Op.Kind_Name);
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
      return (if Kind (Receiver) = Kind_Node
              then Node_Data.Access_Data
                     (Ctx, Node_Val (Receiver), Node.F_Member)
              else Primitives.Data (Receiver, Member_Name));
   exception
      when Unsupported_Error =>
         Raise_Invalid_Member (Ctx, Node, Receiver);
   end Eval_Dot_Access;

   -------------
   -- Eval Is --
   -------------

   function Eval_Is
     (Ctx : Eval_Context; Node : L.Is_Clause) return Primitive
   is
      Tested_Node   : constant Primitive :=
        Eval (Ctx, Node.F_Node_Expr, Kind_Node);
      Type_Name : constant String := To_UTF8 (Node.F_Kind_Name.Text);
   begin
      return To_Primitive
        (Matches_Kind_Name (Type_Name, Node_Val (Tested_Node)));
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
      use Patterns.Nodes.Node_Iterators;
      Current_Node : Iterator_Node;
      Iter         : Filter_Iter := Make_Query_Iterator (Ctx, Node);
      Result       : constant Primitive :=  Make_Empty_List;
   begin
      while Iter.Next (Current_Node) loop
         Append (Result, To_Primitive (Current_Node.Node));
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
   end Eval_Indexing;

   -----------------------------
   -- Eval_List_Comprehension --
   -----------------------------

   function Eval_List_Comprehension
     (Ctx : Eval_Context; Node : L.List_Comprehension) return Primitive
   is
      Comprehension_Envs : constant Comprehension_Env_Iter :=
        Make_Comprehension_Environment_Iter (Ctx, Node.F_Generators);
      Guard_Filter : constant Comprehension_Guard_Filter :=
        Make_Guard_Filter (Ctx, Node.F_Guard);
      Filtered_Envs : constant Environment_Iters.Filter_Iter :=
        Environment_Iters.Filter (Comprehension_Envs, Guard_Filter);
      Comprehension_Closure : constant Closure :=
        Make_Closure (Ctx, Node.F_Expr);
      Comprehension_Values : Env_Primitive_Maps.Map_Iter :=
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

   -------------------
   -- Eval_Val_Expr --
   -------------------

   function Eval_Val_Expr
     (Ctx : Eval_Context; Node : L.Val_Expr) return Primitive
   is
      Binding : Environment_Map;
      Binding_Name  : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node.F_Binding_Name.Text);
      Binding_Value : constant Primitive :=
        Eval (Ctx, Node.F_Binding_Value);
   begin
      Binding.Include (Binding_Name, Binding_Value);
      return Eval (Ctx, Node.F_Expr, Local_Bindings => Binding);
   end Eval_Val_Expr;

   ------------------
   -- Eval_Fun_Def --
   ------------------

   function Eval_Fun_Def
     (Ctx : Eval_Context; Node : L.Fun_Def) return Primitive
   is
      Identifier : constant Text_Type := Node.F_Name.Text;
      Fun_Value  : constant Primitive := To_Primitive (Node);
   begin
      if Ctx.Exists_In_Local_Env (Identifier) then
         Raise_Already_Existing_Symbol
           (Ctx, To_Unbounded_Text (Identifier), Node.F_Name.As_LKQL_Node);
      end if;

      Ctx.Add_Binding (Identifier, Fun_Value);

      return Make_Unit_Primitive;
   end Eval_Fun_Def;

   -------------------
   -- Eval_Fun_Call --
   -------------------

   function Eval_Fun_Call
     (Ctx : Eval_Context; Node : L.Fun_Call) return Primitive
   is
      Arguments : L.Expr_List renames Node.F_Arguments;
      Fun_Def   : L.Fun_Def;
      Fun_Prim  : constant Primitive :=
        Eval_Identifier (Ctx, Node.F_Name);
      Fun_Ctx   : constant Eval_Context :=
        (if Ctx.Is_Root_Context then Ctx else Ctx.Parent_Context);
   begin
      if Kind (Fun_Prim) /= Kind_Fun then
         Raise_Invalid_Kind
           (Ctx, Node.F_Name.As_LKQL_Node, Kind_Fun, Fun_Prim);
      end if;

      Fun_Def := Fun_Val (Fun_Prim);

      if Arguments.Children_Count /= Fun_Def.F_Parameters.Children_Count
      then
         Raise_Invalid_Arity (Ctx, Node, Fun_Def.F_Parameters.Children_Count);
      end if;

      for I in Arguments.First_Child_Index .. Arguments.Last_Child_Index loop
         declare
            Arg       : constant L.LKQL_Node := Arguments.Children (I);
            Arg_Name  : constant Text_Type :=
              Fun_Def.F_Parameters.Children (I).Text;
            Arg_Value : constant Primitive := Eval (Ctx, Arg);
         begin
            Fun_Ctx.Add_Binding (Arg_Name, Arg_Value);
         end;
      end loop;

      return Eval (Fun_Ctx, Fun_Def.F_Body_Expr);
   end Eval_Fun_Call;

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

   -----------------------
   -- Matches_Type_Name --
   -----------------------

   function Matches_Kind_Name
     (Kind_Name : String; Node : LAL.Ada_Node) return Boolean
   is
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

   ------------------
   -- Make_Closure --
   ------------------

   function Make_Closure
     (Ctx : Eval_Context; Body_Expr : L.Expr) return Closure
   is
   begin
      return Closure'(Ctx, Body_Expr);
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

end Interpreter.Evaluation;
