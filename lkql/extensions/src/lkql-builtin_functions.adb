------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
