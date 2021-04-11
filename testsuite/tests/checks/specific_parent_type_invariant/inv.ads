package Pack1 is
   type PT1 is tagged private;
   type PT2 is tagged private
     with Type_Invariant => Invariant_2 (PT2);

   function Invariant_2   (X : PT2) return Boolean;

private
   type PT1 is tagged record
      I : Integer;
   end record;

   type PT2 is tagged record
      I : Integer;
   end record;

   type PT1_N is new PT1 with null record;
   type PT2_N is new PT2 with null record;    --  FLAG
end Pack1;

with Pack1;
package Inv is
   type N_PT1 is new Pack1.PT1 with private;
   type N_PT2 is new Pack1.PT2 with private;  --  FLAG
private
   type N_PT1 is new Pack1.PT1 with null record;
   type N_PT2 is new Pack1.PT2 with null record;
end Inv;
