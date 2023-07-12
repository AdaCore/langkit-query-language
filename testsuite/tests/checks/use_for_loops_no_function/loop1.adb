procedure Loop1 (Str : in out String) is
   J, K, L : Integer;

   function F (Str : String) return Integer is (1);

begin
   J := 0;
   while J <= F (Str) loop   -- NOFLAG
      if Str (J) = ' ' then
         Str (J) := 'a';
      end if;

      J := J + 1;
   end loop;
end Loop1;
