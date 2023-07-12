procedure Member (A, B : Integer) is
   Bool : Boolean;
   subtype S is Integer range 1 .. B;
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

   if A = 0 or (A in Natural and A = 2) then  -- NOFLAG
      null;
   end if;

   if A = 0            -- NOFLAG
     or (A >= 3 and A < 5)
   then
      null;
   end if;

   declare
      function "or" (A, B : Boolean) return Boolean is (True);
   begin
      if A = 0            -- NOFLAG
        or A = 2
      then
         null;
      end if;
   end;

   Bool := A = B or (A >= 1 and A <= B);  --  FLAG
   Bool := A = 100 or A in S;             --  FLAG
   Bool := A = 100 or A in 1 .. B;        --  FLAG
end Member;
