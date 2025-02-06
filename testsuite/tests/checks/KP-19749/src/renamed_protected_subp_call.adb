pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

with Protected_Type; use Protected_Type;

procedure Renamed_Protected_Subp_Call is
begin
   Renamed_Proc;
end Renamed_Protected_Subp_Call;
