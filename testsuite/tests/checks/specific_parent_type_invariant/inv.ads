with Pack1;
package Inv is
   type N_PT1 is new Pack1.PT1 with private;
   type N_PT2 is new Pack1.PT2 with private;  --  FLAG
private
   type N_PT1 is new Pack1.PT1 with null record;
   type N_PT2 is new Pack1.PT2 with null record;
end Inv;
