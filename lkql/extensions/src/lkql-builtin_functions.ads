with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package LKQL.Builtin_Functions is

   function Eval_Print
     (Ctx : access constant Eval_Context; Expr : L.Expr) return Primitive;
   function Eval_Debug
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;
   function Eval_To_List
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

end LKQL.Builtin_Functions;
