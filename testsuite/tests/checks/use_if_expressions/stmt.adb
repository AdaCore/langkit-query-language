function Stmt (X : in out Integer) return Integer is
   Y : Integer;
begin
   if X = 1 then   --  FLAG
      return 1;
   else
      return 2;
   end if;

   if X = 1 then   --  NOFLAG
      null;
   else
      null;
   end if;

   if X = 1 then   --  FLAG
      return 1;
   elsif X = 2 then
      return 2;
   else
      return 3;
   end if;

   if X = 1 then   --  NOFLAG
      return 1;
   elsif X = 2 then
      X := 2;
   else
      return 3;
   end if;

   if X >= 2 then   --  NOFLAG
      X := X + 1;
   elsif X <= 0 then
      Y := X - 1;
   else
      X := 0;
   end if;

   if X >= 2 then   --  FLAG
      X := X + 1;
   elsif X <= 0 then
      X := X - 1;
   else
      X := 0;
   end if;

   return X;
end Stmt;
