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
   I := J + K;                          --  NOFLAG
   I := "abs" (K);                      --  NOFLAG
   I := "*" (J, Right => K);            --  NOFLAG

   A := not B and (C or (A xor B));     --  NOFLAG
   B := "not" (C);                      --  NOFLAG

   --  Call to attribute function, not flagged
   K := Integer'Max (I, J);             --  NOFLAG

   --  Entry call
   T.E1 (10, 11);                       --  FLAG (2)
   T.E2 (True) (10, 11);                --  FLAG (2)
   T.E3 (True);                         --  NOFLAG

   --  Function calls
   I := Fun1 (J);                       --  NOFLAG because ALL is not set
   I := Fun2 (J, K);                    --  FLAG (2)
   I := Fun2_Default (J);               --  FLAG
   I := Fun2 (I => 1, J => 2);          --  NOFLAG

   --  Procedure calls
   Proc1 (I);                           --  NOFLAG because ALL is not set
   Proc2 (I);                           --  FLAG
   Proc2 (I => 1, B => A);              --  NOFLAG

   --  Prefix notation
   Var_T.T_Proc1;                       --  NOFLAG

   Var_T.T_Proc2 (12);                  --  NOFLAG because ALL is not set
   Var_T.T_Proc2 (Y => 10);             --  NOFLAG

   Var_T.T_Proc3 (10);                  --  FLAG
   Var_T.T_Proc3 (Y => 10);             --  NOFLAG
end Pos;

