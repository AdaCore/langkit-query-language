package body Main is
   task body Tsk is
   begin
      null;
   end Tsk;

   protected body Prot is
   end Prot;

   type Arr_Priv is array (1 .. 10) of aliased Priv_Rec  --  FLAG
     with Pack;
end Main;
