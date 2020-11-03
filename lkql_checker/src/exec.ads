with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL; use LKQL;
with LKQL.Primitives; use LKQL.Primitives;

package Exec is

   function LKQL_Eval
     (Context     : Eval_Context;
      LKQL_Script : String;
      LKQL_Context : L.Analysis_Context :=
        L.No_Analysis_Context) return Primitive;
   --  Evaluate the script in the given context and display the error
   --  messages, if any.

   function LKQL_Eval
     (LKQL_Script  : String;
      LKQL_Context : L.Analysis_Context :=
        L.No_Analysis_Context) return Primitive;
   --  Evaluate the script in the given context and display the error
   --  messages, if any.

end Exec;
