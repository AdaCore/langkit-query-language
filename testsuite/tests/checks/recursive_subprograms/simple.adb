function Factorial (N : Natural) return Positive is  --  FLAG
begin
   if N = 0 then
      return 1;
   else
      return N * Factorial (N - 1);
   end if;
end Factorial;
