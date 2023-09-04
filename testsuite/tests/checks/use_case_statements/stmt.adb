procedure Stmt is
   type Enum is (OK, Partial, KO);
   Result, Result2 : Enum;

   procedure Proc (I, L, R : in out Integer) is
   begin
      if I < 1 then    --  NOFLAG
         I := I + 1;
      elsif I in L .. R then
         I := I - 1;
      else
         I := 0;
      end if;
   end Proc;

   type Rec is record
      Field : Integer;
   end record;

   function Func1 (Item : Rec) return String is
   begin
      if Item.Field < 0 then  --  FLAG
         return "aa";
      elsif Item.Field = 0 then
         return "bb";
      else
         return "cc";
      end if;
   end Func1;

begin
   if Result = OK then   --  FLAG
      null;
   elsif Result = Partial then
      null;
   elsif Result = KO then
      null;
   end if;

   if Result = OK then   --  NOFLAG
      null;
   else
      null;
   end if;

   if Result = OK then   --  NOFLAG
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
      if I = 1 then      --  NOFLAG
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
      if F >= 1.0 then   --  NOFLAG
         I := I + 1;
      elsif F <= 0.0 then
         I := I - 1;
      else
         I := 0;
      end if;
   end;
end Stmt;
