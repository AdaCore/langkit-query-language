package Pack is
   type PARENT_0 is range 1 .. 10;
   type Parent_1 is (a, B, C);        --  FLAG 2 times

   package Inner is
      type Parent_2 is tagged null record;   --  FLAG
      AaAa    : exception;
      BbBb    : exception;                   --  FLAG
      Ccc_Ccc : exception;
   end;


   type Parent_3 is tagged private;           --  FLAG

   subtype PARENT_0_S is Parent_0;
   subtype PARENT_1_S is Parent_1;
   subtype PARENT_2_S is Inner.Parent_2;
   subtype PARENT_3_s is Parent_3;            --  FLAG

   C1 : constant Integer := 1;                --  FLAG
   c2 : constant Integer := 2;
   A111 : constant Integer := 3;

   type AAAA is range 1 .. 10;               --  FLAG

private
   type Parent_3 is tagged null record;      --  FLAG
end Pack;
