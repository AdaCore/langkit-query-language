with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

private package LKQL.Functions is

   function Eval_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a call, which can be to either a user defined function or
   --  selector, or to a built-in function.

end LKQL.Functions;
