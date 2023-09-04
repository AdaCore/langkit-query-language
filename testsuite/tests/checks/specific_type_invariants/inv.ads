package Inv is

   type PT is private
   with Type_Invariant => Test_PT (PT);            --  NOFLAG
   function Test_PT (X : PT) return Boolean;

   type TPT1 is tagged private
   with Type_Invariant => Test_TPT1 (TPT1);        --  FLAG
   function Test_TPT1 (X : TPT1) return Boolean;

   type TPT2 is tagged private
   with Type_Invariant'Class => Test_TPT2 (TPT2);  --  NOFLAG
   function Test_TPT2 (X : TPT2) return Boolean;

private
   type PT is tagged null record;
   type TPT1 is tagged null record;
   type TPT2 is tagged null record;
end Inv;
