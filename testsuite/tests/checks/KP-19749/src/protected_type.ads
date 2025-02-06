pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

package Protected_Type is
   protected type P is
      procedure Proc;
   end P;

   P_Obj : P;
   procedure Renamed_Proc renames P_Obj.Proc;
end Protected_Type;
