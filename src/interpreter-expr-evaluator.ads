with Interpreter.Expr; use Interpreter.Expr;

with Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

with Liblkqllang.Analysis;
with Liblkqllang.Common;
with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Expr.Evaluator is
   
   EvalError: exception;
   
   package LEL renames Liblkqllang.Analysis;
   package LELCO renames Liblkqllang.Common;
      
   package String_Value_Map is new
     Ada.Containers.Indefinite_Hashed_Maps 
       (Key_Type => Unbounded_Text_Type,
        Element_Type => Atom,
        Hash => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
        Equivalent_Keys => "=");
   use String_Value_Map;
   
   type EvalCtx is record 
      Env: String_Value_Map.Map := String_Value_Map.Empty_Map;
   end record;

   function Eval (Ctx: in out EvalCtx; 
                  Node: in  LEL.lkql_Node'Class) return Atom;
   
private
   
   function Eval_List (Ctx: in out EvalCtx; Node: in LEL.lkql_Node_List) return Atom;
   function Eval_Assign (Ctx: in out EvalCtx; Node: in LEL.Assign) return Atom;
   function Eval_Identifier (Ctx: in out EvalCtx; Node: in LEL.Identifier) return Atom;
   function Eval_Number (Ctx: in out EvalCtx; Node: in LEL.Number) return Atom;
   function Eval_String_Literal (Ctx: in out EvalCtx; Node: in LEL.String_Literal) return Atom;
   function Eval_Print (Ctx: in out EvalCtx; Node: in LEL.Print_Stmt) return Atom;
   function Eval_Bin_Op (Ctx: in out EvalCtx; Node: in LEL.Bin_Op) return Atom;
   
end Interpreter.Expr.Evaluator;
