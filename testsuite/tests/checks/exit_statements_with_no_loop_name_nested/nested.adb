procedure Bar (I, J : in out Integer) is
begin
   loop
      loop
         exit when I < J;  --  FLAG
         I := I - 1;
         J := J + 1;
      end loop;
   end loop;

   loop
      exit when I < J;  --  NO FLAG
      I := I - 1;
      J := J + 1;
   end loop;
end Bar;
