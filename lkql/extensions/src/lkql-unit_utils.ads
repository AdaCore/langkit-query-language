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

package LKQL.Unit_Utils is

   Unit_Creation_Error : exception;

   procedure Run_Preprocessor
     (Context : Eval_Context; Unit : L.Analysis_Unit);
   --  Run LKQL preprocessor on the unit, which associates precomputed data
   --  to key nodes in order to accelerate runtime evaluation. This phase is
   --  required for all units that must be evaluated. It is already performed
   --  by the routines defined below.

   function Make_Lkql_Unit
     (Eval_Ctx : Eval_Context;
      Path     : String) return L.Analysis_Unit;
   --  Create an LKQL analysis unit in the context 'Context' from the given
   --  file.

   function Make_Lkql_Unit_From_Code
     (Eval_Ctx  : Eval_Context;
      Lkql_Code : String;
      Unit_Name : String := "[inline code]") return L.Analysis_Unit;
   --  Create an LKQL analysis unit in the context 'Context' from the given
   --  Lkql_Code.

private

   function Unit_Or_Error (Unit : L.Analysis_Unit) return L.Analysis_Unit;
   --  If 'Unit' has diagnostics raise a Unit_Create_Error, otherwise return
   --  'Unit'.

end LKQL.Unit_Utils;
