with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package LKQL.Checks is

   procedure Check (Ctx : Eval_Context; Node : L.LKQL_Node'Class);
   --  Verrify that the given node contains valid LKQL code.
   --  An exception will be raised on the first error.

   procedure Check_Fun_Call (Ctx : Eval_Context; Node : L.Fun_Call);
   --  Check a Fun_Call node

private

   procedure Check_Fun_Call_Arguments (Ctx : Eval_Context; Node : L.Fun_Call);

end LKQL.Checks;
