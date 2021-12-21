procedure Stmt is
   type Enum is (OK, Partial, KO);
   Result, Result2 : Enum;

   procedure Proc (I, L, R : in out Integer) is
   begin
      if I < 1 then    --  NO FLAG
         I := I + 1;
      elsif I in L .. R then
         I := I - 1;
      else
         I := 0;
      end if;
   end Proc;

begin
   if Result = OK then   --  FLAG
      null;
   elsif Result = Partial then
      null;
   elsif Result = KO then
      null;
   end if;

   if Result = OK then   --  FLAG
      null;
   elsif Result in Partial .. Partial then
      null;
   elsif Result in KO then
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

   declare
      F : Float;
      I : Integer;
   begin
      if F >= 1.0 then   --  NO FLAG
         I := I + 1;
      elsif F <= 0.0 then
         I := I - 1;
      else
         I := 0;
      end if;
   end;
end Stmt;
