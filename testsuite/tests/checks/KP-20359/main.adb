package body Main is
   procedure Test is
      T_Obj            : T;
      T_Class_Obj      : T'Class := T'Class (D_T'(others => 42));  -- FLAG
      D_T_Class_Obj    : D_T'Class :=
        D_T'Class (D_D_T'(D_T'(I => 10) with J => 20));  -- FLAG
      T_Not_Tagged_Obj : T_Not_Tagged;
   begin
      T_Obj := T (D_T'(others => 42));    -- FLAG
      T_Obj := T (D_T'((others => 42)));  -- FLAG
      T_Obj := T (S_D_T'(others => 42));  -- FLAG
      T_Obj := T (P_D_T'(others => 42));  -- FLAG

      T_Not_Tagged_Obj :=
        T_Not_Tagged (D_T_Not_Tagged'(others => 42));  -- NOFLAG
   end Test;
end Main;
