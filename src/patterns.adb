package body Patterns is

   ------------------------
   -- Make_Match_Success --
   ------------------------

   function Make_Match_Success
     (Bindings : Environment := String_Value_Maps.Empty_Map)
      return Match_Result
   is
      (Match_Result'(Success => True, others => <>));

end Patterns;
