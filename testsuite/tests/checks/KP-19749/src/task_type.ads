pragma Task_Dispatching_Policy (FIFO_Within_Priorities);   --  NOFLAG

package Task_Type is
   task type T is
      entry E;
   end T;

   T_Obj : T;
   procedure Renamed_E renames T_Obj.E;
end Task_Type;
