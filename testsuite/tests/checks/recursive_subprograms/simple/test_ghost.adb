procedure Test_Ghost is

   --  This is ghost, so don't flag by default
   function Factorial (N : Natural) return Positive is  -- NOFLAG
      (if N = 0 then 1 else N * Factorial (N - 1)) with Ghost;

begin
   null;
end Test_Ghost;
