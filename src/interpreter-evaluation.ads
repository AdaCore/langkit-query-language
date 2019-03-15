with Interpreter.Primitives;       use Interpreter.Primitives;
with Interpreter.Eval_Contexts;    use Interpreter.Eval_Contexts;

with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Evaluation is

   function Eval
     (Ctx : in out Eval_Context; Node : LEL.LKQL_Node'Class) return Primitive;
   --  Return the result of the AST node's evaluation in the given context.
   --  An Eval_Error will be raised if the node represents an invalid query or
   --  expression.

   function Typed_Eval (Ctx           : in out Eval_Context;
                        Node          : LEL.LKQL_Node'Class;
                        Expected_Kind : Primitive_Kind) return Primitive;
   --  Evaluate the given node and raise an exception if the kind of the
   --  result doesn't match 'Expected_Kind".

   function To_Ada_Node_Kind
     (Kind_Name : Text_Type) return LALCO.Ada_Node_Kind_Type;
   --  Return the Ada_Node_Kind_Type that matches the given name.
   --  Raise a program error if the name doesn't correspond to any
   --  ada_Node_Kind_Type.

end Interpreter.Evaluation;
