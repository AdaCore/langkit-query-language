with Interpreter.Primitives;    use Interpreter.Primitives;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Analysis;

package Functions is

   package L renames Liblkqllang.Analysis;

   function Eval_Fun_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a function call

private

   function Eval_User_Fun_Call (Ctx  : Eval_Context;
                                Call : L.Fun_Call;
                                Def  : L.Fun_Def) return Primitive;
   --  Evaluate a call to a user-defined function

   function Eval_Arguments (Ctx       : Eval_Context;
                            Arguments : L.Named_Arg_Array)
                            return Environment_Map;
   --  Return a Map containing the argument's value along with their
   --  name.

   procedure Check_Arguments (Ctx       : Eval_Context;
                              Arguments : L.Arg_List;
                              Def       : L.Fun_Def);
   --  Given a function definition and a list of arguments, check that the
   --  function can be applied to the given arguments, which means that:
   --     * The number of arguments matches the arity of the function
   --     * There is no positonnal argument following a named argument
   --     * Named arguments are unique and match the name of a function
   --       parameter

   function Eval_Builtin_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive;
   --  Evaluate a call to a built-in function

   function Eval_Print (Ctx : Eval_Context; Expr : L.Expr) return Primitive;
   --  Evaluate a call to the 'print' built-in function

   function Eval_Debug (Ctx : Eval_Context; Node : L.Expr) return Primitive;
   --  Evaluate a call to the 'debug' built-in function

end Functions;
