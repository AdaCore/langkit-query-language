pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

with Task_Type; use Task_Type;

procedure Renamed_Entry_Call is
begin
   Renamed_E;
end Renamed_Entry_Call;
