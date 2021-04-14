procedure Paths (I, J : in out Integer) is
begin
   if I > 10 then
      J := 5;
   elsif I > 0 then
      null;                 --  FLAG
      null;
   else
     J := J + 1;
   end if;

   case J is
      when 1 =>
         I := I + 1;
      when 2 =>
         null;              --  FLAG
      when 3 =>
         J := J + 1;
      when others =>
         null;              --  FLAG
   end case;
end Paths;
