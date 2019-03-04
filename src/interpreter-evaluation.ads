with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

with Libadalang.Common; use type Libadalang.Common.Ada_Node_Kind_Type;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Interpreter.Evaluation is

   Eval_Error : exception;

   package String_Kind_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => LALCO.Ada_Node_Kind_Type,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");

   function Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Primitive;
   --  Return the result of the AST node's evaluation in the given context.
   --  An Eval_Error will be raised if the node represents an invalid query or
   --  expression.

end Interpreter.Evaluation;
