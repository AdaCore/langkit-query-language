procedure Expanded_Loop_Exit_Names (S : in out String) is
begin
   Search : for J in S'Range loop
      if S (J) = ' ' then
         S (J) := '_';
         exit Expanded_Loop_Exit_Names.Search;            --  FLAG
      end if;
   end loop Search;
end;
