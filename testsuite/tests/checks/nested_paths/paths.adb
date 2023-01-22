procedure Paths (Cond : Boolean) is
   I : Integer;
begin
   if Cond then
      return;
   else
      null;   --  FLAG
   end if;

   if Cond then
      raise Constraint_Error;   --  NO FLAG
   else
      return;                   --  NO FLAG
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
         I := 1;    --  NO FLAG
      end if;

      if Cond then
         return;
      else
         exit when Cond;   --  FLAG
      end if;
   end loop;

   if Cond then
      begin                        --  NO FLAG
         raise Constraint_Error;
      end;
   else
      return;
   end if;

   if Cond then
      declare                      --  NO FLAG
      begin
         raise Constraint_Error;
      end;
   else
      return;
   end if;

   if Cond then
      begin                        --  NO FLAG
         declare
         begin
            raise Constraint_Error;
         end;
      end;
   else
      return;
   end if;
end Paths;
