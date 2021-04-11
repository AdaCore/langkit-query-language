procedure Pos (Ch1, Ch2 : Character; I : in out Integer) is
begin
   if Character'Pos (Ch1) in 32 .. 126           --  FLAG
     and then
      Character'Pos (Ch2) not in 0 .. 31         --  FLAG
   then
      I := (Character'Pos (Ch1) + Character'Pos (Ch2)) / 2;  --  FLAG (twice)
   end if;
end Pos;
