with Interpreter.Errors;           use Interpreter.Errors;
with Interpreter.Error_Handling;   use Interpreter.Error_Handling;

with Libadalang.Iterators;     use Libadalang.Iterators;
with Libadalang.Common;        use type Libadalang.Common.Ada_Node_Kind_Type;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Interpreter.Evaluation is

   package String_Kind_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => LALCO.Ada_Node_Kind_Type,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");

   function Eval_List
     (Ctx : in out Eval_Context; Node : LEL.Expr_List) return Primitive;

   function Eval_Assign
     (Ctx : in out Eval_Context; Node : LEL.Assign) return Primitive;

   function Eval_Identifier
     (Ctx : in out Eval_Context; Node : LEL.Identifier) return Primitive;

   function Eval_Integer (Node : LEL.Integer) return Primitive;

   function Eval_String_Literal (Node : LEL.String_Literal) return Primitive;

   function Eval_Bool_Literal (Node : LEL.Bool_Literal) return Primitive;

   function Eval_Print
     (Ctx : in out Eval_Context; Node : LEL.Print_Stmt) return Primitive;

   function Eval_Bin_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Non_Short_Circuit_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Short_Circuit_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Dot_Access
     (Ctx : in out Eval_Context; Node : LEL.Dot_Access) return Primitive;

   function Eval_Is
     (Ctx : in out Eval_Context; Node : LEL.Is_Clause) return Primitive;

   function Eval_In
     (Ctx : in out Eval_Context; Node : LEL.In_Clause) return Primitive;

   function Eval_Filtered_query (Ctx : in out Eval_Context;
                                 Node : LEL.Filtered_Query)
                                 return Primitive;

   function Eval_Indexing
     (Ctx : in out Eval_Context; Node : LEL.Indexing) return Primitive;

   function To_Ada_Node_Kind
     (Kind_Name : Unbounded_Text_Type) return LALCO.Ada_Node_Kind_Type;

   function Format_Ada_Kind_Name (Name : String) return Unbounded_Text_Type
     with Pre => Name'Length > 4 and then
                 Name (Name'First .. Name'First + 3) = "ADA_";
   --  Takes the String representation of an Ada node kind of the form
   --  "ADA_KIND_NAME" and returns a String of the form "KindName".

   function Init_Name_Kinds_Lookup return String_Kind_Maps.Map;
   --  Fill the Name_Kinds lookup table by asscoaiting a kind name to a
   --  Ada_Node_Kind_Type value.

   procedure Check_Kind (Ctx           : in out Eval_Context;
                         Node          : LEL.LKQL_Node;
                         Expected_Kind : Primitive_Kind;
                         Value         : Primitive);
   --  Raise an exception and register an error in the evaluation context if
   --  `Value` doesn't have the expected kind.

   function Typed_Eval (Ctx           : in out Eval_Context;
                        Node          : LEL.LKQL_Node'Class;
                        Expected_Kind : Primitive_Kind) return Primitive;
   --  Evaluate the given node and raise an exception if the kind of the
   --  result doesn't match 'Expected_Kind".

   function Bool_Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node) return Boolean;
   --  Evalauate the given node and convert to result to an Ada Boolean.
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

   procedure Check_Kind (Ctx           : in out Eval_Context;
                         Node          : LEL.LKQL_Node;
                         Expected_Kind : Primitive_Kind;
                         Value         : Primitive)
   is
   begin
      if Kind (Value) /= Expected_Kind then
         Raise_Invalid_Kind (Ctx, Node, Expected_Kind, Value);
      end if;
   end Check_Kind;

   ----------------
   -- Typed_Eval --
   ----------------

   function Typed_Eval (Ctx           : in out Eval_Context;
                        Node          : LEL.LKQL_Node'Class;
                        Expected_Kind : Primitive_Kind) return Primitive
   is
      Result : constant Primitive := Eval (Ctx, Node);
   begin
      Check_Kind (Ctx, Node.As_LKQL_Node, Expected_Kind, Result);
      return Result;
   end Typed_Eval;

   ---------------
   -- Bool_Eval --
   ---------------

   function Bool_Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node) return Boolean
   is
      Result : constant Primitive := Typed_Eval (Ctx, Node, Kind_Bool);
   begin
      return Bool_Val (Result);
   end Bool_Eval;

   ----------
   -- Eval --
   ----------

   function Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Primitive
   is
   begin
      return (case Node.Kind is
                 when LELCO.lkql_Expr_List =>
                   Eval_List (Ctx, Node.As_Expr_List),
                 when LELCO.lkql_Assign =>
                   Eval_Assign (Ctx, Node.As_Assign),
                 when LELCO.lkql_Identifier =>
                   Eval_Identifier (Ctx, Node.As_Identifier),
                 when LELCO.lkql_Integer =>
                   Eval_Integer (Node.As_Integer),
                 when LELCO.lkql_String_Literal =>
                   Eval_String_Literal (Node.As_String_Literal),
                 when LELCO.lkql_Bool_Literal =>
                   Eval_Bool_Literal (Node.As_Bool_Literal),
                 when LELCO.lkql_Print_Stmt =>
                   Eval_Print (Ctx, Node.As_Print_Stmt),
                 when LELCO.lkql_Bin_Op =>
                   Eval_Bin_Op (Ctx, Node.As_Bin_Op),
                 when LELCO.lkql_Dot_Access =>
                   Eval_Dot_Access (Ctx, Node.As_Dot_Access),
                 when LELCO.lkql_Is_Clause =>
                   Eval_Is (Ctx, Node.As_Is_Clause),
                 when LELCO.lkql_In_Clause =>
                   Eval_In (Ctx, Node.As_In_Clause),
                 when LELCO.lkql_Filtered_Query =>
                   Eval_Filtered_query (Ctx, Node.As_Filtered_Query),
                 when LELCO.lkql_Indexing =>
                   Eval_Indexing (Ctx, Node.As_Indexing),
                 when others =>
                    raise Program_Error
                      with "Invalid evaluation root kind: " & Node.Kind_Name);
   end Eval;

   ---------------
   -- Eval_List --
   ---------------

   function Eval_List
     (Ctx : in out Eval_Context; Node : LEL.Expr_List) return Primitive
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
     (Ctx : in out Eval_Context; Node : LEL.Assign) return Primitive
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

   -------------------------
   -- Eval_Bool_Literal --
   -------------------------

   function Eval_Bool_Literal (Node : LEL.Bool_Literal) return Primitive is
      use type LELCO.LKQL_Node_Kind_Type;
      Value : constant Boolean := (Node.Kind = LELCO.lkql_Bool_Literal_True);
   begin
      return To_Primitive (Value);
   end Eval_Bool_Literal;

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print
     (Ctx : in out Eval_Context; Node : LEL.Print_Stmt) return Primitive
   is
   begin
      Display (Eval (Ctx, Node.F_Value));
      return Make_Unit_Primitive;
   end Eval_Print;

   -----------------
   -- Eval_Bin_Op --
   -----------------

   function Eval_Bin_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
   begin
      return (case Node.F_Op.Kind is
                 when LELCO.lkql_Op_And
                    | LELCO.lkql_Op_Or
                 =>
                    Eval_Short_Circuit_Op (Ctx, Node),
                 when others =>
                    Eval_Non_Short_Circuit_Op (Ctx, Node));
   end Eval_Bin_Op;

   -------------------------------
   -- Eval_Non_Short_Circuit_Op --
   -------------------------------

   function Eval_Non_Short_Circuit_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
      Left   : constant Primitive := Eval (Ctx, Node.F_Left);
      Right  : constant Primitive := Eval (Ctx, Node.F_Right);
   begin
      return (case Node.F_Op.Kind is
              when LELCO.lkql_Op_Plus   => Left + Right,
              when LELCO.lkql_Op_Minus  => Left - Right,
              when LELCO.lkql_Op_Mul    => Left * Right,
              when LELCO.lkql_Op_Div    => Left / Right,
              when LELCO.lkql_Op_Eq     => "=" (Left, Right),
              when LELCO.lkql_Op_Neq    => Left /= Right,
              when LELCO.lkql_Op_Concat => Left & Right,
              when others =>
                 raise Program_Error with
                   "Not a non-short-cirtcuit operator kind: " &
                   Node.F_Op.Kind_Name);
   end Eval_Non_Short_Circuit_Op;

   ---------------------------
   -- Eval_Short_Circuit_Op --
   ---------------------------

   function Eval_Short_Circuit_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive
   is
      Result  : Boolean;
      Left    : constant LEL.LKQL_Node := Node.F_Left.As_LKQL_Node;
      Right   : constant LEL.LKQL_Node := Node.F_Right.As_LKQL_Node;
   begin
      case Node.F_Op.Kind is
      when LELCO.lkql_Op_And =>
         Result :=
           Bool_Eval (Ctx, Left) and then Bool_Eval (Ctx, Right);
      when LELCO.lkql_Op_Or =>
         Result :=
           Bool_Eval (Ctx, Left) or else Bool_Eval (Ctx, Right);
      when others =>
         raise Program_Error
           with "Not a short-circuit operator kind: " & Node.F_Op.Kind_Name;
      end case;

      return To_Primitive (Result);
   end Eval_Short_Circuit_Op;

   --------------------
   -- Eval_Dot_Acess --
   --------------------

   function Eval_Dot_Access
     (Ctx : in out Eval_Context; Node : LEL.Dot_Access) return Primitive
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
     (Ctx : in out Eval_Context; Node : LEL.Is_Clause) return Primitive
   is
      Tested_Node   : constant Primitive :=
        Typed_Eval (Ctx, Node.F_Node_Expr, Kind_Node);
      Expected_Kind : constant LALCO.Ada_Node_Kind_Type :=
        To_Ada_Node_Kind (To_Unbounded_Text (Node.F_Kind_Name.Text));
      LAL_Node      : constant LAL.Ada_Node := Node_Val (Tested_Node);
   begin
      return To_Primitive (LAL_Node.Kind = Expected_Kind);
   end Eval_Is;

   -------------
   -- Eval_In --
   -------------

   function Eval_In
     (Ctx : in out Eval_Context; Node : LEL.In_Clause) return Primitive
   is
      Tested_Value : constant Primitive := Eval (Ctx, Node.F_Value_Expr);
      Tested_List  : constant Primitive :=
        Typed_Eval (Ctx, Node.F_List_Expr, Kind_List);
   begin
      return To_Primitive (Contains (Tested_List, Tested_Value));
   end Eval_In;

   -----------------------------
   -- Eval_Non_Filtered_Query --
   -----------------------------

   function Eval_Filtered_query (Ctx : in out Eval_Context;
                                 Node : LEL.Filtered_Query)
                                 return Primitive
   is
      Current_Node  : LAL.Ada_Node;
      It            : Traverse_Iterator'Class := Traverse (Ctx.AST_Root);
      Result        : constant Primitive := Make_Empty_List (Kind_Node);
      Binding       : constant Unbounded_Text_Type :=
        To_Unbounded_Text
          (Node.F_Query_Pattern.As_Node_Query_Pattern
           .F_Queried_Node.As_Binding_Node_Pattern.F_Binding.Text);
      Env_Conflict  : constant Boolean := Ctx.Env.Contains (Binding);
      Env_Backup    : constant Primitive :=
        (if Env_Conflict then Ctx.Env (Binding) else Make_Unit_Primitive);
   begin
      while It.Next (Current_Node) loop
         Ctx.Env.Include (Binding, To_Primitive (Current_Node));
         declare
            When_Clause_Result : Primitive;
         begin
            When_Clause_Result :=
              Typed_Eval (Ctx, Node.F_Predicate, Kind_Bool);
            if Bool_Val (When_Clause_Result) then
               Append (Result, To_Primitive (Current_Node));
            end if;
         exception
            when Recoverable_Error =>
               null;
         end;
      end loop;

      if Env_Conflict then
         Ctx.Env.Include (Binding, Env_Backup);
      end if;

      return Result;
   end Eval_Filtered_query;

   -------------------
   -- Eval_Indexing --
   -------------------

   function Eval_Indexing
     (Ctx : in out Eval_Context; Node : LEL.Indexing) return Primitive
   is
      List  : constant Primitive :=
        Typed_Eval (Ctx, Node.F_Collection_Expr, Kind_List);
      Index : constant Primitive :=
        Typed_Eval (Ctx, Node.F_Index_Expr, Kind_Int);
   begin
      return Get (List, Int_Val (Index));
   end Eval_Indexing;

   ----------------------
   -- To_Ada_Node_Kind --
   ----------------------

   function To_Ada_Node_Kind
     (Kind_Name : Unbounded_Text_Type) return LALCO.Ada_Node_Kind_Type
   is
      use String_Kind_Maps;
      Position : constant Cursor := Name_Kinds.Find (Kind_Name);
   begin
      if not Has_Element (Position) then
         raise Program_Error with
           "Invalid kind name: " & To_UTF8 (To_Text (Kind_Name));
      end if;

      return Element (Position);
   end To_Ada_Node_Kind;

end Interpreter.Evaluation;
