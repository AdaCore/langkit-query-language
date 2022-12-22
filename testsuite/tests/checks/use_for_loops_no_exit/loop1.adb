procedure Loop1 (Str : in out String) is
   J, K, L : Integer;
begin
   J := 0;
   while J <= Str'Last loop   --  NO FLAG
      if Str (J) = ' ' then
         Str (J) := 'a';
         exit;
      end if;

      J := J + 1;
   end loop;

   K := 0;
   Loop1:
   while K <= Str'Last loop   --  NO FLAG
      if Str (K) = ' ' then
         Str (K) := 'a';
         exit Loop1;
      end if;

      K := K + 1;
   end loop Loop1;

   L := 0;
   Loop2:
   while L <= Str'Last loop   --  FLAG
      loop
         exit;
      end loop;

      L := L + 1;
   end loop Loop2;
end Loop1;
