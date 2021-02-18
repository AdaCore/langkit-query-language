with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

private package LKQL.Functions is

   function Eval_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a call, which can be to either a user defined function or
   --  selector, or to a built-in function.

private

   function Eval_User_Fun_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Func : Primitive) return Primitive;
   --  Evaluate a call to a user-defined function

   function Eval_Arguments
     (Ctx       : Eval_Context;
      Arguments : L.Named_Arg_Array) return Environment_Map;
   --  Return a Map containing the argument's value along with their
   --  name.

   function Eval_Builtin_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a call to a built-in function

end LKQL.Functions;
