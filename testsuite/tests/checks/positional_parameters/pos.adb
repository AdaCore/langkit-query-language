with Pack; use Pack;

procedure Pos is
   I, J, K : Integer := 1;
   A, B, C : Boolean := True;

   F : Float;

   Var_T : Tagged_T := Tagged_T'(null record);

   procedure Proc1 (I : Integer) with Import;
   procedure Proc2 (I : Integer; B : Boolean := True) with Import;

   function Fun1 (I : Integer) return Integer with Import;
   function Fun2 (I : Integer; J : Integer) return Integer with Import;
   function Fun2_Default (I : Integer; J : Integer := 1) return Integer
     with Import;

begin

   --  Calls to operator functions, not flagged
   I := J + K;                          -- NO FLAG
   I := "abs" (K);                      -- NO FLAG
   I := "*" (J, Right => K);            -- NO FLAG

   A := not B and (C or (A xor B));     -- NO FLAG
   B := "not" (C);                      -- NO FLAG

   --  Call to attribute function, not flagged
   K := Integer'Max (I, J);             -- NO FLAG

   --  Entry call
   T.E1 (10, 11);                       --  FLAG twice

   --  Function calls
   I := Fun1 (J);                       --  FLAG if ALL is set
   I := Fun2 (J, K);                    --  FLAG twice
   I := Fun2_Default (J);               --  FLAG
   I := Fun2 (I => 1, J => 2);          --  NO FLAG

   --  Procedure calls
   Proc1 (I);                           --  FLAG if ALL is set
   Proc2 (I);                           --  FLAG
   Proc2 (I => 1, B => A);              --  NO FLAG

   --  Prefix notation
   Var_T.T_Proc1;                       --  NO FLAG

   Var_T.T_Proc2 (12);                  --  FLAG if ALL is set
   Var_T.T_Proc2 (Y => 10);             --  NO FLAG

   Var_T.T_Proc3 (10);                  --  FLAG
   Var_T.T_Proc3 (Y => 10);             --  NO FLAG
end Pos;

