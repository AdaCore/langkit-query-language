pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

with Task_Type; use Task_Type;

procedure Task_Entry_Call is
begin
   T_Obj.E;
end Task_Entry_Call;
