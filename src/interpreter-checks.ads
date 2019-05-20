with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

package Interpreter.Checks is

   procedure Check (Ctx : Eval_Context; Node : L.LKQL_Node'Class);
   --  Verrify that the given node contains valid LKQL code.
   --  An exception will be raised on the first error.

   procedure Check_Fun_Call (Ctx : Eval_Context; Node : L.Fun_Call);

end Interpreter.Checks;
