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

with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package LKQL.Builtin_Functions is

   function Eval_Print
     (Ctx : access constant Eval_Context; Expr : L.Expr) return Primitive;

   function Eval_Debug
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

   function Eval_To_List
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

   function Eval_Dump
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

end LKQL.Builtin_Functions;
