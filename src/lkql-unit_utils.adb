with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;

package body LKQL.Unit_Utils is

   --------------------
   -- Make_LKQL_Unit --
   --------------------

   function Make_LKQL_Unit
     (Path : String; Context : out L.Analysis_Context) return L.Analysis_Unit
   is
   begin
      Context := L.Create_Context;
      return Make_LKQL_Unit (Context, Path);
   end Make_LKQL_Unit;

   --------------------
   -- Make_LKQL_Unit --
   --------------------

   function Make_LKQL_Unit
     (Context : L.Analysis_Context; Path : String) return L.Analysis_Unit
   is (Unit_Or_Error (Context.Get_From_File (Path)));

   ------------------------------
   -- Make_LKQL_Unit_From_Code --
   ------------------------------

   function Make_LKQL_Unit_From_Code
     (LKQL_Code : String) return L.Analysis_Unit
   is (Make_LKQL_Unit_From_Code (L.Create_Context, LKQL_Code));

   ------------------------------
   -- Make_LKQL_Unit_From_Code --
   ------------------------------

   function Make_LKQL_Unit_From_Code (Context   : L.Analysis_Context;
                                      LKQL_Code : String)
                                      return L.Analysis_Unit
   is
     (Unit_Or_Error
        (Context.Get_From_Buffer
             (Filename => "[inline_code]", Buffer   => LKQL_Code)));

   ------------------------
   -- Format_Diagnostics --
   ------------------------

   function Format_Diagnostics (Unit : L.Analysis_Unit) return String is
      use Ada.Characters.Latin_1;
      Result : Unbounded_String;
   begin
      for D of Unit.Diagnostics loop
         Result := Result & Unit.Format_GNU_Diagnostic (D) & LF;
      end loop;

      return To_String (Result);
   end Format_Diagnostics;

   -------------------
   -- Unit_Or_Error --
   -------------------

   function Unit_Or_Error (Unit : L.Analysis_Unit) return L.Analysis_Unit is
   begin
      if Unit.Has_Diagnostics then
         raise Unit_Creation_Error with Format_Diagnostics (Unit);
      end if;
      Unit.Populate_Lexical_Env;
      return Unit;
   end Unit_Or_Error;

end LKQL.Unit_Utils;
