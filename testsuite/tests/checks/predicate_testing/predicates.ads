package Predicates is
   function Ident (I : Integer) return Integer is (I);

   --  subtype Even is Integer with Dynamic_Predicate => Even mod 2 = 0;
   --  disabled pending U401-032

   subtype Even is Integer with Dynamic_Predicate => True;

   subtype Small_Even is Even range -100 .. 100;

   Obj : Small_Even;

   B1 : Boolean := Ident (101) in Even;            --  FLAG
   B2 : Boolean := Ident (101) in Small_Even;      --  FLAG
   B3 : Boolean := Obj'Valid;                      --  FLAG
end Predicates;
