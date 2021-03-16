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

with LKQL.AST_Nodes;
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
        L.No_Analysis_Context;
      Expected_Kind  : Base_Primitive_Kind := No_Kind) return Primitive
   is
   begin
      return Eval
        (Context,
         Make_LKQL_Unit_From_Code (LKQL_Context, LKQL_Script).Root,
         Expected_Kind);
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
        (LKQL.AST_Nodes.Empty_Ast_Node_Array,
         Make_Ada_AST_Node (No_Ada_Node));
   begin
      return LKQL_Eval (Ctx, LKQL_Script, LKQL_Context);
   end LKQL_Eval;

end Exec;
