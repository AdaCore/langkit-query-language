with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;

with Libadalang.Analysis; use Libadalang.Analysis;
with Ada_AST_Nodes; use Ada_AST_Nodes;

package body Exec is

   ---------------
   -- LKQL_Eval --
   ---------------

   function LKQL_Eval
     (Context      : Eval_Context;
      LKQL_Script  : String;
      LKQL_Context : L.Analysis_Context :=
        L.No_Analysis_Context) return Primitive
   is
   begin
      return Check_And_Eval
        (Context,
         Make_LKQL_Unit_From_Code (LKQL_Context, LKQL_Script).Root);
   end LKQL_Eval;

   ---------------
   -- LKQL_Eval --
   ---------------

   function LKQL_Eval
     (LKQL_Script : String;
      LKQL_Context : L.Analysis_Context :=
        L.No_Analysis_Context) return Primitive
   is
      Ctx : constant Eval_Context := Make_Eval_Context
        (Make_Ada_AST_Node (No_Ada_Node),
         Make_Ada_AST_Node (No_Ada_Node));
   begin
      return LKQL_Eval (Ctx, LKQL_Script, LKQL_Context);
   end LKQL_Eval;

end Exec;
