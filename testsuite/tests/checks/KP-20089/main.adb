package body Main is
   task body Tsk is
   begin
      null;
   end Tsk;

   protected body Prot is
      function Get return Integer is
      begin
         return 0;
      end Get;
   end Prot;

   procedure Test is
      B : Base := 1;
      TR : Tag_Rec;
      T : Tsk;
      Pro : Prot;
      LR : Lim_Rec;
      Pri : Priv;
      R : Rec;
      A : Arr;
      ER : Ext_Rec;
   begin
      Test_Out (With_Pred (B));          --  FLAG
      Test_In_Out (With_Pred (B));       --  FLAG
      Test_In_Out (Sub_With_Pred (B));   --  FLAG
      Test_In_Out (With_Stat_Pred (B));  --  FLAG
      Test_In_Out (With_Dyn_Pred (B));   --  FLAG

      Test_In_Out (Base (B));           --  NOFLAG (No predicate)
      Test_Default (With_Pred (B));     --  NOFLAG (Default param mode)
      Test_In (With_Pred (B));          --  NOFLAG (In param mode)
      Test_In_Out (B);                  --  NOFLAG (No conversion)
      Test_Tag_Rec (Sub_Tag_Rec (TR));  --  NOFLAG (By ref type)
      Test_Tsk (Sub_Tsk (T));           --  NOFLAG (By ref type)
      Test_Sub_Tsk (Sub_Sub_Tsk (T));   --  NOFLAG (By ref type)
      Test_Prot (Sub_Prot (Pro));       --  NOFLAG (By ref type)
      Test_Lim_Rec (Sub_Lim_rec (LR));  --  NOFLAG (By ref type)
      Test_Priv (Sub_Priv (Pri));       --  NOFLAG (By ref type)
      Test_Rec (Sub_Rec (R));           --  NOFLAG (By ref type)
      Test_Arr (Sub_Arr (A));           --  NOFLAG (By ref type)
      Test_Ext_Rec (Sub_Ext_Rec (ER));  --  NOFLAG (By ref type)
   end Test;
end Main;
