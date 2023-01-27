package Interfaces is

   type Queue is limited interface;                                   --  NO FLAG
   type Synchronized_Queue is synchronized interface and Queue;       --  FLAG
   type Synchronized_Task is task interface;                          --  FLAG
   type Synchronized_Protected is protected interface;                --  FLAG

end Interfaces;
