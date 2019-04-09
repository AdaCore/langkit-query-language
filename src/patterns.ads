with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Analysis;
with Liblkqllang.Common;

with Libadalang.Analysis;

package Patterns is

   package L renames Liblkqllang.Analysis;
   package LCO renames Liblkqllang.Common;

   package LAL renames Libadalang.Analysis;

   type Match_Result is record
      Success  : Boolean;
      --  True if the matching attempt succeeded
      Bindings : Environment_Map := String_Value_Maps.Empty_Map;
      --  If the pattern contains a binding name and the match is successul,
      --  the matched value will be stored in this environment, associated with
      --  the binding name.
   end record;
   --  Represents that result of a matching attempt

   Match_Failure : constant Match_Result :=
     Match_Result'(Success => False, others => <>);
   --  Special value representing the failure of a matching attempt

   function Make_Match_Success
     (Bindings : Environment_Map := String_Value_Maps.Empty_Map)
      return Match_Result;
   --  Create a Match_Result value representing a successful matching attempt
   --  with the given binding(s).

end Patterns;
