pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

with Task_Type; use Task_Type;

procedure Abort_Stmt is
begin
   abort T_Obj;
end Abort_Stmt;
