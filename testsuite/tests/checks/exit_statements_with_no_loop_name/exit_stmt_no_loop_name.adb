procedure Bar (I, J : in out Integer) is
begin
   loop
      exit when I < J;  --  FLAG
      I := I - 1;
      J := J + 1;
   end loop;
end Bar;
