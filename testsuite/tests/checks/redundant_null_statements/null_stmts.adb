procedure Null_Stmts is
begin
   null;      --  NO FLAG
   null;      --  FLAG

   begin
      null;   --  NO FLAG
   end;

   loop
      null;   --  NO FLAG
   end loop;

   loop
      pragma Assert (True);
      null;   --  FLAG
   end loop;

   begin
      null;   --  FLAG
      pragma Assert (True);
   end;

   <<label>> null;  --  NO FLAG (label)
end;
