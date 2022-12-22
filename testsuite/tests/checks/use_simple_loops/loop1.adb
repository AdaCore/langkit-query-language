procedure Loop1 (Str : in out String) is
   J : Integer;
begin
   J := 0;
   while True loop   --  FLAG
      if Str (J) = ' ' then
         Str (J) := 'a';
         return;
      end if;

      J := J + 1;
   end loop;

   declare
      function True return Boolean is (False);
   begin
      while True loop         --  NO FLAG
         null;
      end loop;
   end;

   while Standard.True loop   --  FLAG
      null;
   end loop;

   while 1 = 1 loop   --  FLAG
      null;
   end loop;
end Loop1;
