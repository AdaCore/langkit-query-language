--  if the rule parameter is 3 or less
procedure Proc (I : in out Integer; S : String) is   --  FLAG
begin
   if I in 1 .. 10 then
      for J in S'Range loop

         if S (J) = ' ' then
            if I > 10 then
               exit;
            else
               I := 10;
            end if;
         end if;

         I := I + Character'Pos (S (J));
      end loop;
   end if;
end Proc;

