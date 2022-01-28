------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with LKQL.Partial_AST_Nodes;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;

with Libadalang.Analysis; use Libadalang.Analysis;
with Ada_AST_Nodes; use Ada_AST_Nodes;

package body Exec is

   ---------------
   -- Lkql_Eval --
   ---------------

   function Lkql_Eval
     (Context      : Eval_Context;
      Lkql_Script  : String;
      Lkql_Context : L.Analysis_Context :=
        L.No_Analysis_Context;
      Expected_Kind  : Base_Primitive_Kind := No_Kind) return Primitive
   is
   begin
      return Eval
        (Context,
         Make_Lkql_Unit_From_Code (Lkql_Context, Lkql_Script).Root,
         Expected_Kind);
   end Lkql_Eval;

   ---------------
   -- Lkql_Eval --
   ---------------

   function Lkql_Eval
     (Lkql_Script : String;
      Lkql_Context : L.Analysis_Context :=
        L.No_Analysis_Context) return Primitive
   is
      Ctx : constant Eval_Context := Make_Eval_Context
        (LKQL.Partial_AST_Nodes.Empty_Ast_Node_Array,
         Make_Ada_AST_Node (No_Ada_Node));
   begin
      return Lkql_Eval (Ctx, Lkql_Script, Lkql_Context);
   end Lkql_Eval;

end Exec;
