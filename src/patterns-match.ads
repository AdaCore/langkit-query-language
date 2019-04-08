with Interpreter.Primitives; use Interpreter.Primitives;

package Patterns.Match is

   function Match_Pattern (Ctx     : Eval_Context;
                           Pattern : L.Base_Pattern;
                           Value   : Primitive) return Match_Result;
   --  Match a Primitive value against the given pattern

   function Match_Unfiltered (Ctx     : Eval_Context;
                              Pattern : L.Unfiltered_Pattern;
                              Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that dosn't contain a
   --  filtering predicate.

   function Match_Filtered (Ctx     : Eval_Context;
                            Pattern : L.Filtered_Pattern;
                            Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that contains a filtering
   --  predicate.

   function Match_Value (Ctx     : Eval_Context;
                         Pattern : L.Value_Pattern;
                         Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a value pattern, i.e a pattern that
   --  doesn't contain a binding name.

   function Match_Binding (Pattern : L.Binding_Pattern;
                           Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a binding pattern, i.e a pattern that
   --  only contains a binding name.

   function Match_Full (Ctx     : Eval_Context;
                        Pattern : L.Full_Pattern;
                        Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that contains both a binding
   --  name and a value pattern.

end Patterns.Match;
