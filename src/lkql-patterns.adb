package body LKQL.Patterns is

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success (Self : Match_Result) return Boolean is
     (Is_Some (Self.Matched_Value));

   -----------------------
   -- Get_Matched_Value --
   -----------------------

   function Get_Matched_Value (Self : Match_Result) return Primitive is
      (Extract (Self.Matched_Value));

   ------------------------
   -- Make_Match_Success --
   ------------------------

   function Make_Match_Success
     (Matched_Value : Primitive;
      Bindings      : Environment_Map := String_Value_Maps.Empty_Map)
      return Match_Result
   is
      (Match_Result'(To_Option (Matched_Value), Bindings));

end LKQL.Patterns;
