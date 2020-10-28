
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
                                      LKQL_Code : String)
                                      return L.Analysis_Unit;
   --  Create an LKQL analysis unit in the context 'Context' from the given
   --  LKQL_Code.

   function Format_Diagnostics (Unit : L.Analysis_Unit) return String
     with Pre => Unit.Has_Diagnostics;
   --  Return a String containing all the diagnostics from 'Unit' separated by
   --  line terminators.

private

   function Unit_Or_Error (Unit : L.Analysis_Unit) return L.Analysis_Unit;
   --  If 'Unit' has diagnostics raise a Unit_Create_Error, otherwise return
   --  'Unit'.

end LKQL.Unit_Utils;
