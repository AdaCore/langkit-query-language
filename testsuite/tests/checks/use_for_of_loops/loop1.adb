procedure Loop1 (Str : in out String) is
begin
   for J in Str'Range loop    --  FLAG
      if Str (J) = ' ' then
         return;
      elsif Str (J) = 'a' then
         exit;
      end if;
   end loop;

   for J in Str'Range loop    --  FLAG
      Str (J) := ' ';
   end loop;

   for J in Str'Range loop    --  NOFLAG
      exit when J > 1;
   end loop;
end Loop1;
