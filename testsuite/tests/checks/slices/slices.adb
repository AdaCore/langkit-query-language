procedure Slices (S : in out String; L, R : Positive) is
   Tmp : String := S (L .. R);        --  FLAG
   T   : String (1 .. 10);            --  NO FLAG
   subtype S10 is String (1 .. 10);   --  NO FLAG

   subtype Ind is Integer range 1 .. 5;

   X : String := T (Ind);             --  FLAG

begin
   null;
end Slices;
