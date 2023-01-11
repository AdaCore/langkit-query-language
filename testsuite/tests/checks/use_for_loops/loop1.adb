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

   declare
      I : Integer := 1;
   begin
      while I < 10 loop           --  NO FLAG
         begin
            I := I + 1;
         end;

         exit;
      end loop;
   end Non_Predefined_Relation;

end Loop1;
