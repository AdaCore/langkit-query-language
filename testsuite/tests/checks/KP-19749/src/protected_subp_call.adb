pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

with Protected_Type; use Protected_Type;

procedure Protected_Subp_Call is
begin
   P_Obj.Proc;
end Protected_Subp_Call;
