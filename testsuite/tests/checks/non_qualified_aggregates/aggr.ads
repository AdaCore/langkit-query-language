package Aggr is

   type Arr is array (1 .. 10) of Integer;

   Var1 : Arr := (1 => 10, 2 => 20, others => 30);       --  FLAG
   Var2 : Arr := Arr'(1 => 10, 2 => 20, others => 30);   --  NOFLAG
   Var3 : array (1 .. 10) of Integer :=
     (1 => 10, 2 => 20, others => 30);                   --  NOFLAG

   type Table is array (Integer range 1 .. 3) of Integer;
   type Table2 is array (Integer range <>) of Integer;
   type Two_Dim_Array is array (1 .. 2, 1 .. 2) of Character;
   type Two_Dim_Array_Ptr is access all Two_Dim_Array;

   t1 : Table := (1, 8, 5);                                    --  FLAG
   t2 : constant Table := (others => 5);                       --  FLAG
   t3 : Table := Table'(1, 2, 3);                              --  NOFLAG
   t4 : Table2 (20 .. 25) := Table2'(20 .. 25 => 100);         --  NOFLAG
   t5 : Two_Dim_Array := (('a', 'b'), ('e', 'f'));             --  FLAG
   t6 : Two_Dim_Array_Ptr :=
      new Two_Dim_Array'((others => 'x'), ('s', 't'));         --  NOFLAG

   type Month_Name is (January, February, March, August);
   type Date is tagged record
      Day   : Integer range 1 .. 31;
      Month : Month_Name;
      Year  : Integer range 0 .. 4000;
   end record;
   type Weather is new Date with record
      Temperature : Integer range -50 .. 50;
   end record;

   dd : Date := (4, January, 2011);                             --  FLAG
   tt : Weather := (2, March, 2011, 30);                        --  FLAG
   tu : Weather := Weather'(2, March, 2011, 30);                --  NOFLAG
   tc : constant Weather := (Date'(26, August, 1974) with 15);  --  FLAG
   td : Weather := Weather'((Date'(26, August, 1974) with 15)); --  NOFLAG

   --  Check that the aggregate inside enum repr clauses cannot be flagged
   type En is (Var1, Var2);
   for En use (Var1 => 12, Var2 => 15); --  NOFLAG

end Aggr;
