pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

procedure Delay_Stmt is
begin
   delay Duration (0);
end Delay_Stmt;
