procedure Loop2 (Str : in out String) is
   J : Integer;

   procedure Nested is
   begin
      J := J + 1;
   end Nested;
begin
   J := 0;
   while J <= Str'Last loop   --  NO FLAG: Nested modifies J
      Nested;

      if Str (J) = ' ' then
         Str (J) := 'a';
         return;
      end if;

      J := J + 1;
   end loop;
end Loop2;
