pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  FLAG
pragma Locking_Policy (Ceiling_Locking);                   --  FLAG
pragma Queuing_Policy (FIFO_Queuing);                      --  FLAG
pragma Discard_Names;                                      --  NOFLAG

with Task_Type;      use Task_Type;
with Protected_Type; use Protected_Type;

procedure Flags is
begin
   null;
end Flags;
