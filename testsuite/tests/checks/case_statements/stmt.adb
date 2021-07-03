procedure Stmt is
   type Enum is (OK, Partial, KO);
   Result, Result2 : Enum;
begin
   if Result = OK then   --  FLAG
      null;
   elsif Result = Partial then
      null;
   elsif Result = KO then
      null;
   end if;

   if Result = OK then   --  NO FLAG
      null;
   else
      null;
   end if;

   if Result = OK then   --  NO FLAG
      null;
   elsif Result2 = OK then
      null;
   else
      null;
   end if;
end Stmt;
