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
with Langkit_Support.Text; use Langkit_Support.Text;

--  This package declares every function that is a builtin in LKQL, and an
--  array of builtin function descriptors that lists all the functions and
--  their name in LKQL.
--
--  To add a new built-in function, you must:
--
--  1. Add a new function with the correct prototype in that package
--
--  2. Register it in the list of built-ins (The ``Builtin_Functions`` array
--     below).

package LKQL.Builtin_Functions is

   function Eval_Print
     (Ctx : access constant Eval_Context; Expr : L.Expr) return Primitive;

   function Eval_Debug
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

   function Eval_To_List
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

   function Eval_Dump
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

   function Eval_Image
     (Ctx : access constant Eval_Context; Node : L.Expr) return Primitive;

   type Builtin_Fn_Desc is record
      Name : Unbounded_Text_Type;
      Fn   : Builtin_Function_Access;
   end record;

   type Builtin_Fn_Array
   is array (Positive range <>) of Builtin_Fn_Desc;

   Builtin_Functions : Builtin_Fn_Array :=
     ((To_Unbounded_Text ("print"), Eval_Print'Access),
      (To_Unbounded_Text ("debug"), Eval_Debug'Access),
      (To_Unbounded_Text ("to_list"), Eval_To_List'Access),
      (To_Unbounded_Text ("dump"), Eval_Dump'Access),
      (To_Unbounded_Text ("img"), Eval_Image'Access));

end LKQL.Builtin_Functions;
