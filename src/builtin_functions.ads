with Interpreter.Primitives;    use Interpreter.Primitives;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Analysis;

package Builtin_Functions is

   package L renames Liblkqllang.Analysis;

   function Is_Builtin_Call (Call : L.Fun_Call) return Boolean;
   --  Return wether 'Call' is a call to a built-in function

   function Eval_Builtin_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a call to a built-in function

private

   function Eval_Print (Ctx : Eval_Context; Expr : L.Expr) return Primitive;

   --  function Eval_Debug
   --  (Ctx : Eval_Context; Value : L.Expr) return Primitive;

end Builtin_Functions;
