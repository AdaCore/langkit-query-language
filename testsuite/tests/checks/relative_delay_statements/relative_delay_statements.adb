procedure Relative_Delay_Statements is
begin
   if I > 0 then
      delay until Current_Time + Big_Delay;
   else
      delay Small_Delay;                      --  FLAG
   end if;
end Relative_Delay_Statements;
