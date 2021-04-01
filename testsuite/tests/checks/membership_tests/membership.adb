procedure Membership (S : in out Integer) is
begin
   if S in 1 .. 5 | 9 then      --  FLAG
      null;
   end if;
end Membership;
