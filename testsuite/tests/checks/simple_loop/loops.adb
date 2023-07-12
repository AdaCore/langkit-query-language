procedure Loops is
begin
   loop                    --  FLAG
      exit when True;
   end loop;

   for J in 1 .. 10 loop   -- NOFLAG
      null;
   end loop;

   while True loop         -- NOFLAG
      null;
   end loop;
end Loops;
