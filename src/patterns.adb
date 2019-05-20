package body Patterns is

   ------------------------
   -- Make_Match_Success --
   ------------------------

   function Make_Match_Success
     (Bindings : Environment_Map := String_Value_Maps.Empty_Map)
      return Match_Result
   is
      (Match_Result'(Success => True, Bindings => Bindings));

end Patterns;
