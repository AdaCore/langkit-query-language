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

package LKQL.Unit_Utils is

   Unit_Creation_Error : exception;

   function Make_LKQL_Unit
     (Path : String; Context : out L.Analysis_Context) return L.Analysis_Unit;
   --  Create an LKQL analysis Unit from the given file

   function Make_LKQL_Unit
     (Context : L.Analysis_Context; Path : String) return L.Analysis_Unit;
   --  Create an LKQL analysis unit in the context 'Context' from the given
   --  file.

   function Make_LKQL_Unit_From_Code
     (LKQL_Code : String) return L.Analysis_Unit;
   --  Create an LKQL analysis unit from the given LKQL code

   function Make_LKQL_Unit_From_Code (Context   : L.Analysis_Context;
                                      LKQL_Code : String;
                                      Unit_Name : String := "[inline code]")
                                      return L.Analysis_Unit;
   --  Create an LKQL analysis unit in the context 'Context' from the given
   --  LKQL_Code.

private

   function Unit_Or_Error (Unit : L.Analysis_Unit) return L.Analysis_Unit;
   --  If 'Unit' has diagnostics raise a Unit_Create_Error, otherwise return
   --  'Unit'.

end LKQL.Unit_Utils;
