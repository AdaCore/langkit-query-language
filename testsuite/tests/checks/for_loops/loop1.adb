procedure Loop1 (Str : in out String) is
   J : Integer;
begin
   J := 0;
   while J <= Str'Last loop   --  FLAG
      if Str (J) = ' ' then
         Str (J) := 'a';
         return;
      end if;

      J := J + 1;
   end loop;
end Loop1;
