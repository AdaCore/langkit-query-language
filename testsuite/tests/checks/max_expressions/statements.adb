procedure Statements is
   I : Integer;

   function F (X : Integer) return Integer is (X);  --  FLAG if N < 1
   function F5 (X, Y : Integer) return Integer is (X + Y);  --  FLAG if N < 2

begin
   I := 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10;  --  FLAG if N < 10
   I := F (I);   --  FLAG if N < 2
   I := F5 (1 + 2 + 3 + 4 + 5, 2, 3, 4, 5);   --  FLAG (twice) if N < 5
end Statements;
