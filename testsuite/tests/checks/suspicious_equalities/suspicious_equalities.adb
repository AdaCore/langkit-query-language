procedure Suspicious_Equalities is
   X : Integer := 0;
begin
   if X = 1 and x = 2 then       --  FLAG
      null;
   end;
   if X = 1 and then x = 2 then  --  FLAG
      null;
   end;
   if X /= 1 or x /= 2 then      --  FLAG
      null;
   end;
   if X /= 1 or else x/= 2 then  --  FLAG
      null;
   end;
end;
