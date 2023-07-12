procedure Null_Stmts is
begin
   null;      -- NOFLAG
   null;      --  FLAG

   begin
      null;   -- NOFLAG
   end;

   loop
      null;   -- NOFLAG
   end loop;

   loop
      pragma Assert (True);
      null;   --  FLAG
   end loop;

   <<label>> null;  -- NOFLAG (label)

   begin
      null;   --  FLAG
      pragma Assert (True);
   end;
end;
