procedure Slices (S : in out String; L, R : Positive) is
   Tmp : String := S (L .. R);        --  FLAG
   T   : String (1 .. 10);            --  NO FLAG
   subtype S10 is String (1 .. 10);   --  NO FLAG

begin
   null;
end Slices;
