with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;
with Interpreter.Types.Atoms; use Interpreter.Types.Atoms;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded; 

with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Evaluation is
   
   EvalError: exception;
      
   package String_Value_Map is new
     Ada.Containers.Indefinite_Hashed_Maps 
       (Key_Type => Unbounded_Text_Type,
        Element_Type => Primitive,
        Hash => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
        Equivalent_Keys => "=");
   use String_Value_Map;
   
   type EvalCtx is record 
      Env: String_Value_Map.Map := String_Value_Map.Empty_Map;
   end record;

   function Eval (Ctx: in out EvalCtx; 
                  Node: in  LEL.lkql_Node'Class) return Primitive;
   
private
   
   function Eval_List (Ctx: in out EvalCtx; Node: in LEL.lkql_Node_List) return Primitive;
   function Eval_Assign (Ctx: in out EvalCtx; Node: in LEL.Assign) return Primitive;
   function Eval_Identifier (Ctx: in out EvalCtx; Node: in LEL.Identifier) return Primitive;
   function Eval_Number (Node: in LEL.Number) return Primitive;
   function Eval_Integer (Node: in LEL.Integer) return Primitive;
   function Eval_String_Literal (Node: in LEL.String_Literal) return Primitive;
   function Eval_Print (Ctx: in out EvalCtx; Node: in LEL.Print_Stmt) return Primitive;
   function Eval_Bin_Op (Ctx: in out EvalCtx; Node: in LEL.Bin_Op) return Primitive;
   
   function Compute_Bin_Op (Op: LEL.Op'Class; Left, Right: Atom) return Atom;
   
   function Reduce(Ctx: in out EvalCtx; Node: in LEL.LKQL_Node'Class) return Atom;
   
end Interpreter.Evaluation;
