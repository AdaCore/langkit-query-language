procedure Num is
   CI1  : constant       := 1 + 2;      --  NO FLAG
   CF1  : constant Float := 1.0 + 2.0;  --  NO FLAG
   Ind1 : constant Positive := 1;       --  NO FLAG
   Ind2 : constant Positive := 2;       --  NO FLAG
   S1   : String (Ind1 .. Ind2);        --  NO FLAG
   S2   : String (1 .. 2);              --  FLAG unless statements_only
   I    : Integer := 3;                 --  FLAG unless statements_only
   F    : Float   := CF1;               --  NO FLAG
begin
   S1 (1) := 'a';                       --  FLAG only with n < 1
   S1 (Ind1) := 'a';                    --  NO FLAG
   S1 (Ind1 + 2) := 'a';                --  FLAG only with n < 2

   F := 1.0;                            --  FLAG
   I := 2 + 0;                          --  FLAG only with n < 2
   I := I + 2;                          --  FLAG only with n < 2
   I := 3 * I;                          --  FLAG
   I := I ** 3;                         --  NO FLAG
   I := S1'First(3);                    --  NO FLAG
end Num;
