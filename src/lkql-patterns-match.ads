
package LKQL.Patterns.Match is

   function Match_Pattern_Array (Ctx      : Eval_Context;
                                 Patterns : L.Base_Pattern_Array;
                                 Value    : Primitive)
                                 return Match_Array_Result;
   --  Match a value agains an array of pattern.
   --  Return the index of the first successful match, allong with the
   --  associated bindings, if any.

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

   function Match_Binding (Ctx     : Eval_Context;
                           Pattern : L.Binding_Pattern;
                           Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that contains both a binding
   --  name and a value pattern.

end LKQL.Patterns.Match;
