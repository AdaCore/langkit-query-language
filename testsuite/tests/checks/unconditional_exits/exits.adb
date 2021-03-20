procedure Find_A (S : String; Idx : out Natural) is
begin
   Idx := 0;

   for J in S'Range loop
      if S (J) = 'A' then
         Idx := J;
         exit;             --  FLAG
      end if;
   end loop;
end Find_A;
