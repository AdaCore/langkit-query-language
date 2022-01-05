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

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL; use LKQL;
with LKQL.Primitives; use LKQL.Primitives;

package Exec is

   function LKQL_Eval
     (Context        : Eval_Context;
      LKQL_Script    : String;
      LKQL_Context   : L.Analysis_Context :=
        L.No_Analysis_Context;
      Expected_Kind  : Base_Primitive_Kind := No_Kind) return Primitive;
   --  Evaluate the script in the given context and display the error
   --  messages, if any.

   function LKQL_Eval
     (LKQL_Script  : String;
      LKQL_Context : L.Analysis_Context :=
        L.No_Analysis_Context) return Primitive;
   --  Evaluate the script in the given context and display the error
   --  messages, if any.

end Exec;
