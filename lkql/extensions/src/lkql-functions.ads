with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

private package LKQL.Functions is

   function Eval_Fun_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a function call

private

   function Eval_User_Fun_Call (Ctx  : Eval_Context;
                                Call : L.Fun_Call;
                                Def  : L.Fun_Decl) return Primitive;
   --  Evaluate a call to a user-defined function

   function Eval_Arguments (Ctx       : Eval_Context;
                            Arguments : L.Named_Arg_Array)
                            return Environment_Map;
   --  Return a Map containing the argument's value along with their
   --  name.

   function Eval_Builtin_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a call to a built-in function

   function Eval_Print (Ctx : Eval_Context; Expr : L.Expr) return Primitive;
   --  Evaluate a call to the 'print' built-in function

   function Eval_Debug (Ctx : Eval_Context; Node : L.Expr) return Primitive;
   --  Evaluate a call to the 'debug' built-in function

end LKQL.Functions;
