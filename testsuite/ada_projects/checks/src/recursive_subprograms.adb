package body Recursive_Subprograms is

   --------
   -- P1 --
   --------

   procedure P1_Call_P2 (N : Integer) is
   begin
      P2_Call_P3 (N);
   end P1_Call_P2;

   --------
   -- P2 --
   --------

   procedure P2_Call_P3 (N : Integer) is
   begin
      P3_Call_P1 (N);
   end P2_Call_P3;

   --------
   -- P3 --
   --------

   procedure P3_Call_P1 (N : Integer) is
   begin
      P1_Call_P2 (N - 1);
   end P3_Call_P1;

   

end Recursive_Subprograms;
