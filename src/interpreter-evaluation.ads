with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;
with Interpreter.Types.Atoms;      use Interpreter.Types.Atoms;

with Libadalang.Common; use type Libadalang.Common.Ada_Node_Kind_Type;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Interpreter.Evaluation is

   Eval_Error : exception;

   package String_Kind_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => LALCO.Ada_Node_Kind_Type,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");

   package String_Value_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_Text_Type, Element_Type => Primitive,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");
   use String_Value_Maps;

   type Eval_Context is record
      Env      : String_Value_Maps.Map;
      --  Store the value associated with variable names.

      AST_Root : LAL.Ada_Node := LAL.No_Ada_Node;
      --  Root node of the tree in wich node queries will run.
   end record;
   --  Store the evaluation context.

   function Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Primitive;
   --  Return the result of the AST node's evaluation in the given context.
   --  An Eval_Error will be raised if the node represents an invalid query or
   --  expression.

private

   function Eval_List
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node_List) return Primitive;

   function Eval_Assign
     (Ctx : in out Eval_Context; Node : LEL.Assign) return Primitive;

   function Eval_Identifier
     (Ctx : in out Eval_Context; Node : LEL.Identifier) return Primitive;

   function Eval_Integer (Node : LEL.Integer) return Primitive;

   function Eval_String_Literal
     (Node : LEL.String_Literal) return Primitive;

   function Eval_Print
     (Ctx : in out Eval_Context; Node : LEL.Print_Stmt) return Primitive;

   function Eval_Bin_Op
     (Ctx : in out Eval_Context; Node : LEL.Bin_Op) return Primitive;

   function Eval_Dot_Access
     (Ctx : in out Eval_Context; Node : LEL.Dot_Access) return Primitive;

   function Eval_Is
     (Ctx : in out Eval_Context; Node : LEL.Is_Clause) return Primitive;

   function Eval_Query
     (Ctx : in out Eval_Context; Node : LEL.Query) return Primitive;

   function To_Ada_Node_Kind
     (Kind_Name : Unbounded_Text_Type) return LALCO.Ada_Node_Kind_Type;

   function Get_Field
     (Name : Text_Type; Node : LAL.Ada_Node) return LAL.Ada_Node;

   function Get_Field_Index
     (Name : Text_Type; Node : LAL.Ada_Node) return Positive;

   function Compute_Bin_Op (Op : LEL.Op'Class; Left, Right : Atom) return Atom;

   function Reduce
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Atom;

end Interpreter.Evaluation;
