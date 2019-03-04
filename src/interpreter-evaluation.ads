with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

package Interpreter.Evaluation is

   function Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Primitive;
   --  Return the result of the AST node's evaluation in the given context.
   --  An Eval_Error will be raised if the node represents an invalid query or
   --  expression.

end Interpreter.Evaluation;
