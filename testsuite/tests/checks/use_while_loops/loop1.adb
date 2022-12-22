procedure Loop1 (Str : in out String) is
   J : Integer;
begin
   J := 0;
   loop           --  FLAG
      exit when J > 10;

      if Str (J) = ' ' then
         Str (J) := 'a';
         return;
      end if;

      J := J + 1;
   end loop;

   loop           --  NO FLAG
      null;
   end loop;

   Outer_Loop:
   loop           --  NO FLAG
      loop        --  NO FLAG
         exit Outer_Loop when True;
      end loop;
   end loop Outer_Loop;

   Local_Loop:
   loop           --  FLAG
      exit Local_Loop;
   end loop Local_Loop;

end Loop1;
