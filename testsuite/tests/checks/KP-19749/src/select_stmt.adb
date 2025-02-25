pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

with Task_Type; use Task_Type;

procedure Select_Stmt is
begin
   select
      T_Obj.E;
   else
      null;
   end select;
end Select_Stmt;
