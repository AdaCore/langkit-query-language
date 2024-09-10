procedure Main is
   function Id (B : Boolean) return Boolean is (B);
   function Id (I : Integer) return Integer is (I);

   type Rec (D : Boolean) is null record;
   subtype Stat_Const_Rec is Rec (True);
   subtype Dyn_Const_Rec is Rec (Id (True));

   type Arr is array (Integer range <>) of Integer;
   subtype Stat_Const_Arr is Arr (1 .. 3);
   subtype Dyn_Const_Arr is Arr (1 .. Id (3));

   subtype Stat_Int is Integer range 1 .. 3;
   subtype Dyn_Int is Integer range Id (1) .. Id (3);

   subtype Stat_Pred_Int is Integer
     with Static_Predicate => Stat_Pred_Int in 1 .. 5;
   subtype Dyn_Pred_Int is Integer
     with Dynamic_Predicate => Dyn_Pred_Int < 50;

   S : String := "Hello";
   C_S : constant String := "world";

   procedure Process_Int (I : Integer) is
   begin
      null;
   end Process_Int;

   procedure Process_Stat_Int (I : Stat_Int) is
   begin
      null;
   end Process_Stat_Int;

   procedure Process_Dyn_Int (I : Dyn_Int) is
   begin
      null;
   end Process_Dyn_Int;

   procedure Process_Stat_Pred_Int (I : Stat_Pred_Int) is
   begin
      null;
   end Process_Stat_Pred_Int;

   procedure Process_Dyn_Pred_Int (I : Dyn_Pred_Int) is
   begin
      null;
   end Process_Dyn_Pred_Int;

   procedure Process_Multiple (I : Dyn_Pred_Int; J : Stat_Pred_Int) is
   begin
      null;
   end Process_Multiple;
begin
   Process_Int (S'Length);              --  NOFLAG
   Process_Int (S'Size);                --  NOFLAG
   Process_Int (C_S'Length);            --  NOFLAG
   Process_Int (C_S'Size);              --  NOFLAG
   Process_Stat_Int (S'Length);         --  NOFLAG
   Process_Stat_Int (S'Size);           --  NOFLAG
   Process_Stat_Int (C_S'Length);       --  NOFLAG
   Process_Stat_Int (C_S'Size);         --  NOFLAG
   Process_Dyn_Int (S'Length);          --  NOFLAG
   Process_Dyn_Int (S'Size);            --  NOFLAG
   Process_Dyn_Int (C_S'Length);        --  FLAG
   Process_Dyn_Int (C_S'Size);          --  NOFLAG
   Process_Stat_Pred_Int (S'Length);    --  NOFLAG
   Process_Stat_Pred_Int (S'Size);      --  NOFLAG
   Process_Stat_Pred_Int (C_S'Length);  --  NOFLAG
   Process_Stat_Pred_Int (C_S'Size);    --  NOFLAG
   Process_Dyn_Pred_Int (S'Length);     --  NOFLAG
   Process_Dyn_Pred_Int (S'Size);       --  NOFLAG
   Process_Dyn_Pred_Int (C_S'Length);   --  FLAG
   Process_Dyn_Pred_Int (C_S'Size);     --  NOFLAG

   Process_Multiple (S'Length, S'Size);      --  NOFLAG
   Process_Multiple (C_S'Length, C_S'Size);  --  FLAG
end Main;
