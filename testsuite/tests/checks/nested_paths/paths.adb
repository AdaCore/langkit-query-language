procedure Paths (Cond : Boolean) is
   I : Integer;
begin
   if Cond then
      return;
   else
      null;   --  FLAG
   end if;

   if Cond then
      raise Constraint_Error;   --  NOFLAG
   else
      return;                   --  FLAG
   end if;

   if Cond then
      return;
   else
      I := 1;       --  FLAG
   end if;

   if Cond then
      I := 1;       --  FLAG
   else
      return;
   end if;

   loop
      if Cond then
         exit;
      else
         I := 1;    --  FLAG
      end if;

      if Cond then
         exit when Cond;
      else
         I := 1;    --  NOFLAG
      end if;

      if Cond then
         return;
      else
         exit when Cond;   --  FLAG
      end if;
   end loop;

   if Cond then
      begin                        --  NOFLAG
         raise Constraint_Error;
      end;
   else
      return;                      -- FLAG
   end if;

   if Cond then
      begin                        --  FLAG
         raise Constraint_Error;
      exception
         when others => null;
      end;
   else
      return;
   end if;

   if Cond then
      declare                      --  NOFLAG
      begin
         raise Constraint_Error;
      end;
   else
      return;                      --  FLAG
   end if;

   if Cond then
      begin                        --  NOFLAG
         declare
         begin
            raise Constraint_Error;
         end;
      end;
   else
      return;                      --  FLAG
   end if;

   if Cond then
      begin
         raise Constraint_Error;
      exception
         when Constraint_Error => null;
      end;
   else
      I := I + 1;                  --  NOFLAG
   end if;

   if Cond then
      return 1;  --  NOFLAG
   else
      return 2;  --  NOFLAG
   end if;

   if Cond then
      return 1;
   else
      exit;      --  FLAG
   end if;

   if Cond then
      goto lbl;
   else
      return 2;  --  FLAG
   end if;

   if Cond then
      begin         --  NOFLAG
         return 1;
      end;
   else
      return 2;     --  NOFLAG
   end if;

   if Cond then
      return 1;
   else
      begin  --  FLAG
         exit;
      end;
   end if;

   <<lbl>>
end Paths;
