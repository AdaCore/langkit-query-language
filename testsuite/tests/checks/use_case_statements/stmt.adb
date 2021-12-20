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

   declare
      function ">" (L, R : Integer) return Boolean is (L + R <= L - R);
      I : Integer;
   begin
      if I = 1 then      --  NO FLAG
         I := I + 1;
      elsif I > 20 then
         I := I + 2;
      else
         I := 0;
      end if;
   end;
end Stmt;
