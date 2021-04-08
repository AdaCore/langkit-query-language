procedure Other (I, J : in out Integer) is
begin
   case J is
      when 1 =>
         I := I + 1;
      when 3 =>
         J := J + 1;
      when others =>        --  FLAG
         null;
   end case;
end Other;
