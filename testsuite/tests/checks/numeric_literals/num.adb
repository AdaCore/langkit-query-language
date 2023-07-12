procedure Num is
   CI1  : constant       := 1 + 2;      --  NOFLAG
   CF1  : constant Float := 1.0 + 2.0;  --  NOFLAG
   Ind1 : constant Positive := 1;       --  NOFLAG
   Ind2 : constant Positive := 2;       --  NOFLAG
   S1   : String (Ind1 .. Ind2);        --  NOFLAG
   S2   : String (1 .. 2);              --  NOFLAG because of statements_only
   I    : Integer := 3;                 --  NOFLAG because of statements_only
   F    : Float   := CF1;               --  NOFLAG
begin
   S1 (1) := 'a';                       --  NOFLAG unless n < 1
   S1 (Ind1) := 'a';                    --  NOFLAG
   S1 (Ind1 + 2) := 'a';                --  NOFLAG unless n < 2

   F := 1.0;                            --  FLAG
   I := 2 + 0;                          --  NOFLAG unless n < 2
   I := I + 2;                          --  NOFLAG unless n < 2
   I := 3 * I;                          --  FLAG
   I := I ** 3;                         --  NOFLAG
   I := S1'First(3);                    --  NOFLAG
end Num;
