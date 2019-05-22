with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

private package LKQL.Patterns is

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

   subtype Match_Index is Integer range Positive'First - 1 .. Positive'Last;

   type Match_Array_Result is record
      Index    :  Match_Index := Match_Index'First;
      --  Index of the first matched pattern
      Bindings : Environment_Map := String_Value_Maps.Empty_Map;
      --  Bindings from the pattern, if any.
   end record;
   --  Represents the result of a matching attempt against a sequence of
   --  patterns.

end LKQL.Patterns;
