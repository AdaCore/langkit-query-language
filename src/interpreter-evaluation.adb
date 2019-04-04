with Query.Patterns;             use Query.Patterns;
with Query.Iterators;            use Query.Iterators;
with Interpreter.Errors;         use Interpreter.Errors;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Libadalang.Iterators;     use Libadalang.Iterators;
with Libadalang.Common;        use type Libadalang.Common.Ada_Node_Kind_Type;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Assertions;                  use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Interpreter.Evaluation is

   package String_Kind_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => LALCO.Ada_Node_Kind_Type,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");

   function Eval_List
     (Ctx : Eval_Context; Node : LEL.Expr_List) return Primitive;

   function Eval_Assign
     (Ctx : Eval_Context; Node : LEL.Assign) return Primitive;

   function Eval_Identifier
     (Ctx : Eval_Context; Node : LEL.Identifier) return Primitive;

   function Eval_Integer (Node : LEL.Integer) return Primitive;

   function Eval_String_Literal (Node : LEL.String_Literal) return Primitive;

   function Eval_Bool_Literal (Node : LEL.Bool_Literal) return Primitive;

   function Eval_Print
     (Ctx : Eval_Context; Node : LEL.Print_Stmt) return Primitive;

   function Eval_Bin_Op
     (Ctx : Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Short_Circuit_Op
     (Ctx : Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Dot_Access
     (Ctx : Eval_Context; Node : LEL.Dot_Access) return Primitive;

   function Eval_Is
     (Ctx : Eval_Context; Node : LEL.Is_Clause) return Primitive;

   function Eval_In
     (Ctx : Eval_Context; Node : LEL.In_Clause) return Primitive;

   function Eval_Query
     (Ctx : Eval_Context; Node : LEL.Query) return Primitive;

   function Eval_Indexing
     (Ctx : Eval_Context; Node : LEL.Indexing) return Primitive;

   function Eval_List_Comprehension
     (Ctx : Eval_Context; Node : LEL.List_Comprehension) return Primitive;

   function Make_Comprehension_Environment_Iter
     (Ctx : Eval_Context; Node : LEL.Arrow_Assoc_List)
      return Comprehension_Env_Iter;
   --  Given a List of Arrow_Assoc, return an iterator that yields the
   --  environments produced by this list of Arrow_Assoc in the context of a
   --  list comprehension.

   function Format_Ada_Kind_Name (Name : String) return Unbounded_Text_Type
     with Pre => Name'Length > 4 and then
                 Name (Name'First .. Name'First + 3) = "ADA_";
   --  Takes the String representation of an Ada node kind of the form
   --  "ADA_KIND_NAME" and returns a String of the form "KindName".

   function Init_Name_Kinds_Lookup return String_Kind_Maps.Map;
   --  Fill the Name_Kinds lookup table by asscoaiting a kind name to a
   --  Ada_Node_Kind_Type value.

   procedure Check_Kind (Ctx           : Eval_Context;
                         Node          : LEL.LKQL_Node;
                         Expected_Kind : Valid_Primitive_Kind;
                         Value         : Primitive);
   --  Raise an exception and register an error in the evaluation context if
   --  `Value` doesn't have the expected kind.

   function Bool_Eval
     (Ctx : Eval_Context; Node : LEL.LKQL_Node) return Boolean;
   --  Evalaluate the given node and convert to result to an Ada Boolean.
   --  Raise an exception if the result of the node's evaluation is not a
   --  boolean.

   --------------------------
   -- Format_Ada_Kind_Name --
   --------------------------

   function Format_Ada_Kind_Name (Name : String) return Unbounded_Text_Type is
      use Ada.Characters.Handling;
      use Ada.Characters.Conversions;
      Formatted : Unbounded_Text_Type;
      New_Word  : Boolean := True;
   begin
      for C of Name (Name'First + 4 .. Name'Last) loop
         if C /= '_' then
            if New_Word then
               Append (Formatted, To_Wide_Wide_Character (C));
            else
               Append (Formatted, To_Wide_Wide_Character (To_Lower (C)));
            end if;

            New_Word := False;
         else
            New_Word := True;
         end if;
      end loop;

      return Formatted;
   end Format_Ada_Kind_Name;
   --  TODO: do the conversion using Langkit's primitives (when available !)

   ----------------------------
   -- Init_Name_Kinds_Lookup --
   ----------------------------

   function Init_Name_Kinds_Lookup return String_Kind_Maps.Map is
      Result : String_Kind_Maps.Map;
   begin
      for K in LALCO.Ada_Node_Kind_Type loop
         Result.Insert (Format_Ada_Kind_Name (K'Image), K);
      end loop;

      return Result;
   end Init_Name_Kinds_Lookup;

   Name_Kinds : constant String_Kind_Maps.Map := Init_Name_Kinds_Lookup;
   --  Lookup table used to quickly retrieve the Ada node kind associated
   --  with a given name, if any.

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind (Ctx           : Eval_Context;
                         Node          : LEL.LKQL_Node;
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
     (Ctx : Eval_Context; Node : LEL.LKQL_Node) return Boolean
   is
      Result : constant Primitive := Eval (Ctx, Node, Kind_Bool);
   begin
      return Bool_Val (Result);
   end Bool_Eval;

   ----------
   -- Eval --
   ----------

   function Eval (Ctx            : Eval_Context;
                  Node           : LEL.LKQL_Node'Class;
                  Expected_Kind  : Base_Primitive_Kind := No_Kind;
                  Local_Bindings : Environment := String_Value_Maps.Empty_Map)
                  return Primitive
   is
      Result             : Primitive;
      Bindings_Conflicts : constant Environment :=
        Backup_Env (Ctx.Env, Local_Bindings);
   begin
      Update_Env (Ctx.Env, Local_Bindings);

      Result :=
        (case Node.Kind is
            when LELCO.LKQL_Expr_List =>
              Eval_List (Ctx, Node.As_Expr_List),
            when LELCO.LKQL_Assign =>
              Eval_Assign (Ctx, Node.As_Assign),
            when LELCO.LKQL_Identifier =>
              Eval_Identifier (Ctx, Node.As_Identifier),
            when LELCO.LKQL_Integer =>
              Eval_Integer (Node.As_Integer),
            when LELCO.LKQL_String_Literal =>
              Eval_String_Literal (Node.As_String_Literal),
            when LELCO.LKQL_Bool_Literal =>
              Eval_Bool_Literal (Node.As_Bool_Literal),
            when LELCO.LKQL_Print_Stmt =>
              Eval_Print (Ctx, Node.As_Print_Stmt),
            when LELCO.LKQL_Bin_Op =>
              Eval_Bin_Op (Ctx, Node.As_Bin_Op),
            when LELCO.LKQL_Dot_Access =>
              Eval_Dot_Access (Ctx, Node.As_Dot_Access),
            when LELCO.LKQL_Is_Clause =>
              Eval_Is (Ctx, Node.As_Is_Clause),
            when LELCO.LKQL_In_Clause =>
              Eval_In (Ctx, Node.As_In_Clause),
            when LELCO.LKQL_Query | LELCO.LKQL_Filtered_Query =>
              Eval_Query (Ctx, Node.As_Query),
            when LELCO.LKQL_Indexing =>
              Eval_Indexing (Ctx, Node.As_Indexing),
            when LELCO.LKQL_List_Comprehension =>
              Eval_List_Comprehension (Ctx, Node.As_List_Comprehension),
            when others =>
               raise Assertion_Error
                 with "Invalid evaluation root kind: " & Node.Kind_Name);
      Update_Env (Ctx.Env, Bindings_Conflicts);

      if Expected_Kind in Valid_Primitive_Kind then
         Check_Kind (Ctx, Node.As_LKQL_Node, Expected_Kind, Result);
      end if;

      return Result;

   exception
      when others =>
         Update_Env (Ctx.Env, Bindings_Conflicts);
         raise;
   end Eval;

   ---------------
   -- Eval_List --
   ---------------

   function Eval_List
     (Ctx : Eval_Context; Node : LEL.Expr_List) return Primitive
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
     (Ctx : Eval_Context; Node : LEL.Assign) return Primitive
   is
      Identifier : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Node.F_Identifier.Text);
   begin
      Ctx.Env.Include (Identifier, Eval (Ctx, Node.F_Value));
      return Make_Unit_Primitive;
   end Eval_Assign;

   ---------------------
   -- Eval_identifier --
   ---------------------

   function Eval_Identifier
     (Ctx : Eval_Context; Node : LEL.Identifier) return Primitive
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

   -------------------------
   -- Eval_Bool_Literal --
   -------------------------

   function Eval_Bool_Literal (Node : LEL.Bool_Literal) return Primitive is
      use type LELCO.LKQL_Node_Kind_Type;
      Value : constant Boolean := (Node.Kind = LELCO.LKQL_Bool_Literal_True);
   begin
      return To_Primitive (Value);
   end Eval_Bool_Literal;

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print
     (Ctx : Eval_Context; Node : LEL.Print_Stmt) return Primitive
   is
   begin
      Display (Eval (Ctx, Node.F_Value));
      return Make_Unit_Primitive;
   end Eval_Print;

   -----------------
   -- Eval_Bin_Op --
   -----------------

   function Eval_Bin_Op
     (Ctx : Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
   begin
      return (case Node.F_Op.Kind is
                 when LELCO.LKQL_Op_And
                    | LELCO.LKQL_Op_Or
                 =>
                    Eval_Short_Circuit_Op (Ctx, Node),
                 when others =>
                    Eval_Non_Short_Circuit_Op (Ctx, Node));
   end Eval_Bin_Op;

   -------------------------------
   -- Eval_Non_Short_Circuit_Op --
   -------------------------------

   function Eval_Non_Short_Circuit_Op
     (Ctx : Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
      Left   : constant Primitive := Eval (Ctx, Node.F_Left);
      Right  : constant Primitive := Eval (Ctx, Node.F_Right);
   begin
      return (case Node.F_Op.Kind is
              when LELCO.LKQL_Op_Plus   => Left + Right,
              when LELCO.LKQL_Op_Minus  => Left - Right,
              when LELCO.LKQL_Op_Mul    => Left * Right,
              when LELCO.LKQL_Op_Div    => Left / Right,
              when LELCO.LKQL_Op_Eq     => "=" (Left, Right),
              when LELCO.LKQL_Op_Neq    => Left /= Right,
              when LELCO.LKQL_Op_Concat => Left & Right,
              when others =>
                 raise Assertion_Error with
                   "Not a non-short-cirtcuit operator kind: " &
                   Node.F_Op.Kind_Name);
   end Eval_Non_Short_Circuit_Op;

   ---------------------------
   -- Eval_Short_Circuit_Op --
   ---------------------------

   function Eval_Short_Circuit_Op
     (Ctx : Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
      Result  : Boolean;
      Left    : constant LEL.LKQL_Node := Node.F_Left.As_LKQL_Node;
      Right   : constant LEL.LKQL_Node := Node.F_Right.As_LKQL_Node;
   begin
      case Node.F_Op.Kind is
      when LELCO.LKQL_Op_And =>
         Result :=
           Bool_Eval (Ctx, Left) and then Bool_Eval (Ctx, Right);
      when LELCO.LKQL_Op_Or =>
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
     (Ctx : Eval_Context; Node : LEL.Dot_Access) return Primitive
   is
      Receiver    : constant Primitive := Eval (Ctx, Node.F_Receiver);
      Member_Name : constant Text_Type := Node.F_Member.Text;
   begin
      return Property (Receiver, Member_Name);
   exception
      when Unsupported_Error =>
         Raise_Invalid_Member (Ctx, Node, Receiver);
   end Eval_Dot_Access;

   -------------
   -- Eval Is --
   -------------

   function Eval_Is
     (Ctx : Eval_Context; Node : LEL.Is_Clause) return Primitive
   is
      Tested_Node   : constant Primitive :=
        Eval (Ctx, Node.F_Node_Expr, Kind_Node);
      Expected_Kind : constant LALCO.Ada_Node_Kind_Type :=
        To_Ada_Node_Kind (Node.F_Kind_Name.Text);
      LAL_Node      : constant LAL.Ada_Node := Node_Val (Tested_Node);
   begin
      return To_Primitive (LAL_Node.Kind = Expected_Kind);
   end Eval_Is;

   -------------
   -- Eval_In --
   -------------

   function Eval_In
     (Ctx : Eval_Context; Node : LEL.In_Clause) return Primitive
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
     (Ctx : Eval_Context; Node : LEL.Query) return Primitive
   is
      use Query.Iterators.Node_Iterators;
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
     (Ctx : Eval_Context; Node : LEL.Indexing) return Primitive
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
     (Ctx : Eval_Context; Node : LEL.List_Comprehension) return Primitive
   is
      Comprehension_Envs : constant Comprehension_Env_Iter :=
        Make_Comprehension_Environment_Iter (Ctx, Node.F_Generators);
      Guard_Filter : constant Comprehension_Guard_Filter :=
        Make_Guard_Filter (Ctx, Node.F_Guard);
      Filtered_Envs : constant Environment_Iters.Filter_Iter :=
        Environment_Iters.Filter (Comprehension_Envs, Guard_Filter);
      Comprehension_Closure : constant Closure :=
        Make_Closure (Ctx, Node.F_Expr);
      Comprehension_Values : constant Env_Primitive_Maps.Map_Iter :=
        (if Node.F_Guard.Is_Null
         then
            Env_Primitive_Maps.Map (Comprehension_Envs, Comprehension_Closure)
         else
            Env_Primitive_Maps.Map (Filtered_Envs, Comprehension_Closure));
   begin
      return To_Primitive (Comprehension_Values);
   end Eval_List_Comprehension;

   function Environment_Iter_For_Assoc
     (Ctx    : Eval_Context;
      Assoc  : LEL.Arrow_Assoc;
      Nested : Comprehension_Env_Iter_Access)
      return Comprehension_Env_Iter_Access;

   -----------------------------------------
   -- Make_Comprehension_Environment_Iter --
   -----------------------------------------

   function Make_Comprehension_Environment_Iter
     (Ctx : Eval_Context; Node : LEL.Arrow_Assoc_List)
      return Comprehension_Env_Iter
   is
      Current_Env : Comprehension_Env_Iter_Access := null;
      Res       : Comprehension_Env_Iter;
   begin
      for I in reverse Node.Children'Range loop
         declare
            Current_Assoc   : constant LEL.Arrow_Assoc :=
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
      Assoc  : LEL.Arrow_Assoc;
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

   ----------------------
   -- To_Ada_Node_Kind --
   ----------------------

   function To_Ada_Node_Kind
     (Kind_Name : Text_Type) return LALCO.Ada_Node_Kind_Type
   is
      use String_Kind_Maps;
      Position : constant Cursor :=
        Name_Kinds.Find (To_Unbounded_Text (Kind_Name));
   begin
      pragma Assert
        (Has_Element (Position), "Invalid kind name: " & To_UTF8 (Kind_Name));

      return Element (Position);
   end To_Ada_Node_Kind;

   function Update_Nested_Env (Iter   : in out Comprehension_Env_Iter;
                               Result : out Environment) return Boolean;
   --  Return a new enviroment built by adding the current iterator's binding
   --  to the environment produced by it's 'Nested' iterator.

   function Create_New_Env (Iter   : in out Comprehension_Env_Iter;
                            Result : out Environment) return Boolean;
   --  Return a new environment containing only the current iterator's binding

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Comprehension_Env_Iter;
                             Result : out Environment) return Boolean
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
                               Result : out Environment) return Boolean
   is
      Env            : Environment;
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
                            Result : out Environment) return Boolean
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
      Primitive_Iters.Free_Iterator (Iter.Gen);
      Free_Resetable_Environement_Iter (Iter.Nested);
   end Release;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate (Self    : in out Closure;
                                 Element : Environment) return Primitive
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
     (Ctx : Eval_Context; Body_Expr : LEL.Expr) return Closure
   is
   begin
      return Closure'(Ctx, Body_Expr);
   end Make_Closure;

   --------------
   -- Evaluate --
   --------------

   function Evaluate (Self : in out Comprehension_Guard_Filter;
                      Element : Environment) return Boolean
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
                               Guard : LEL.Expr)
                               return Comprehension_Guard_Filter
   is
   begin
      return Comprehension_Guard_Filter'(Ctx, Guard);
   end Make_Guard_Filter;

end Interpreter.Evaluation;
