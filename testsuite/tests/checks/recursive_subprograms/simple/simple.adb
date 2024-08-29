function Simple (N : Natural) return Positive is  --  FLAG
begin
   if N = 0 then
      return 1;
   else
      return N * Simple (N - 1);
   end if;
end Simple;
