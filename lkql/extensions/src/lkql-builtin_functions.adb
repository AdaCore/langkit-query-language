with LKQL.Evaluation; use LKQL.Evaluation;
with Langkit_Support.Text; use Langkit_Support.Text;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada_AST_Nodes; use Ada_AST_Nodes;

package body LKQL.Builtin_Functions is

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print
     (Ctx : access constant Eval_Context; Expr : L.Expr) return Primitive
   is
   begin
      Display (Eval (Ctx.all, Expr));
      return Make_Unit_Primitive;
   end Eval_Print;

   ----------------
   -- Eval_Debug --
   ----------------

   function Eval_Debug
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive
   is
      Code    : constant Text_Type := Node.Text;
      Value   : constant Primitive := Eval (Ctx.all, Node);
      Message : constant Unbounded_Text_Type :=
        Code & " = " & To_Unbounded_Text (Value);
   begin
      Put_Line (To_Text (Message));
      return Value;
   end Eval_Debug;

   ------------------
   -- Eval_To_List --
   ------------------

   function Eval_To_List
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive
   is
      Value : constant Primitive := Eval (Ctx.all, Node, Kind_Iterator);
   begin
      return To_List (Value.Get.Iter_Val.all);
   end Eval_To_List;

   ---------------
   -- Eval_Dump --
   ---------------

   function Eval_Dump
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive
   is
      Value : constant Primitive := Eval (Ctx.all, Node, Kind_Node);
   begin
      Ada_AST_Node (Value.Get.Node_Val.Unchecked_Get.all).Node.Print;
      return Make_Unit_Primitive;
   end Eval_Dump;

end LKQL.Builtin_Functions;
