procedure Member (A : Integer) is
begin
   if A = 0            --  FLAG
     or A in Natural
     or A = 2
     or (A >= 3 and A <= 5)
   then
      null;
   end if;

   if A = 0            --  FLAG if short_circuit
     or else (A >= 3 and A <= 5)
   then
      null;
   end if;

   if A = 0 or A in Natural and A = 2 then  --  NO FLAG
      null;
   end if;

   if A = 0            --  NO FLAG
     or (A >= 3 and A < 5)
   then
      null;
   end if;

   declare
      function "or" (A, B : Boolean) return Boolean is (True);
   begin
      if A = 0            --  NO FLAG
        or A = 2
      then
         null;
      end if;
   end;
end Member;
