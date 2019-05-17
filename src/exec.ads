with Interpreter.Primitives;    use Interpreter.Primitives;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Analysis;

with Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

package Exec is

   package L renames Liblkqllang.Analysis;

   package LAL renames Libadalang.Analysis;

   function Load
     (Path : String; Context : out L.Analysis_Context) return Eval_Context;
   --  Create an empty evaluation context and use it to
   --  run the LKQL program contained  in the file designated by 'Path',
   --  then return the evaluation Context.
   --  Loading the program requires the creation of an analysis context that
   --  will be stored in the out 'Context' parameter.
   --  An exception will be raised if the LKQL program is ill-formed or the
   --  file at 'Path' doesn't exist.

   function Load (Script : L.LKQL_Node) return Eval_Context;
   --  Create an empty evaluation context and use it to evaluate the given
   --  node, then return the evaluation Context.

   function Lookup
     (Ctx : Eval_Context; Name : Unbounded_Text_Type) return Primitive;
   --  Return the Primitive value associated to the given name in the
   --  evaluation context.
   --  Raise an exception if the value doesn't exist.

   function Call_Function (Ctx       : Eval_Context;
                           Name      : Unbounded_Text_Type;
                           Arguments : Environment_Map) return Primitive;
   --  Call the function named 'Name' with the given arguments.

   function Eval_LKQL_Code (LKQL_Code : String) return Primitive;
   --  Evaluate a String containing LKQL code.
   --  An exception will be raised if 'LKQL_Code' doesn't contain valid
   --  LKQL code.

   function Eval_LKQL_Code
     (Ctx : Eval_Context; LKQL_Code : String) return Primitive;
   --  Evaluate a String containing LKQL code in the given evaluation context.
   --  An exception will be raised if 'LKQL_Code' doesn't contain valid
   --  LKQL code.

end Exec;
