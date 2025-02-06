pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

package body Protected_Type is
   protected body P is
      procedure Proc is
      begin
         null;
      end Proc;
   end P;
end Protected_Type;
