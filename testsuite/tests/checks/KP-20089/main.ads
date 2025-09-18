package Main is
   procedure Test;

   --  Simple range type
   type Base is range 1 .. 10;
   subtype With_Pred is Base with Predicate => True;
   subtype Sub_With_Pred is With_Pred;
   subtype With_Stat_Pred is Base with Static_Predicate => True;
   subtype With_Dyn_Pred is Base with Dynamic_Predicate => True;

   procedure Test_Default (X : Base) is null;
   procedure Test_In (X : in Base) is null;
   procedure Test_Out (X : out Base) is null;
   procedure Test_In_Out (X : in out Base) is null;

   --  Tagged record
   type Tag_Rec is tagged null record;
   subtype Sub_Tag_Rec is Tag_Rec with Predicate => True;

   procedure Test_Tag_Rec (X : in out Tag_Rec) is null;

   --  Task type
   task type Tsk;
   subtype Sub_Tsk is Tsk with Predicate => True;
   subtype Sub_Sub_Tsk is Sub_Tsk with Predicate => True;

   procedure Test_Tsk (X : in out Tsk) is null;
   procedure Test_Sub_Tsk (X : in out Sub_Tsk) is null;

   --  Protected type
   protected type Prot is
      function Get return Integer;
   end Prot;
   subtype Sub_Prot is Prot with Predicate => True;

   procedure Test_Prot (X : in out Prot) is null;

   --  Limited record type
   type Lim_Rec is limited null record;
   subtype Sub_Lim_rec is Lim_Rec with Predicate => True;

   procedure Test_Lim_Rec (X : in out Lim_Rec) is null;

   --  Private type with by reference full view
   type Priv is private;
   subtype Sub_Priv is Priv with Predicate => True;

   procedure Test_Priv (X : in out Priv) is null;

   --  Composite type with by reference component
   type Rec is record
      P : Priv;
   end record;
   subtype Sub_Rec is Rec with Predicate => True;

   type Arr is array (1 .. 1) of Priv;
   subtype Sub_Arr is Arr with Predicate => True;

   type Ext_Rec is new Tag_Rec with null record;
   subtype Sub_Ext_Rec is Ext_Rec with Predicate => True;

   procedure Test_Rec (X : in out Rec) is null;
   procedure Test_Arr (X : in out Arr) is null;
   procedure Test_Ext_Rec (X : in out Ext_Rec) is null;
private
   type Priv is tagged null record;
end Main;
