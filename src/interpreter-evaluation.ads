with Interpreter.Primitives;       use Interpreter.Primitives;
with Interpreter.Eval_Contexts;    use Interpreter.Eval_Contexts;

with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Evaluation is

   function Eval (Ctx            : Eval_Context_Ptr;
                  Node           : LEL.LKQL_Node'Class;
                  Expected_Kind  : Base_Primitive_Kind := No_Kind;
                  Local_Bindings : Environment := String_Value_Maps.Empty_Map)
                  return Primitive;
   --  Return the result of the AST node's evaluation in the given context.
   --  An Eval_Error will be raised if the node represents an invalid query or
   --  expression.

   function To_Ada_Node_Kind
     (Kind_Name : Text_Type) return LALCO.Ada_Node_Kind_Type;
   --  Return the Ada_Node_Kind_Type that matches the given name.
   --  Raise a program error if the name doesn't correspond to any
   --  ada_Node_Kind_Type.

end Interpreter.Evaluation;
