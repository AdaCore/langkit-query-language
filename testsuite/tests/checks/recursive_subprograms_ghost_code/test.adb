procedure Test is
   --  This is ghost code, but we passed the option to analyze ghost code, so
   --  flag
   function Factorial (N : Natural) return Positive is  -- FLAG
      (if N = 0 then 1 else N * Factorial (N - 1)) with Ghost;
begin
   null;
end Test;
