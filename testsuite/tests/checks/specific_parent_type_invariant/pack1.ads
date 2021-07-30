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
