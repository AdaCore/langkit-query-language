procedure Loops is
begin
   loop                    --  FLAG
      exit when True;
   end loop;

   for J in 1 .. 10 loop   --  NO FLAG
      null;
   end loop;

   while True loop         --  NO FLAG
      null;
   end loop;
end Loops;
