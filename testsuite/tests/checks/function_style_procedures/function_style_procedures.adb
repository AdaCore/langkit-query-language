package body Function_Style_Procedures is

   procedure Glob (R : out Integer) is
   begin
      R := 0;
      Global_Var := 1;
   end Glob;

   procedure P (R : out Integer) is
   begin
      R := 0;
   end P;

   procedure P_Limited (R : out Lim) is
   begin
      null;
   end P_Limited;

   procedure P_Non_Limited_Rec (R : out Non_Limited_Rec) is null;

   procedure P2 (R : out Integer; R2: in out Integer) is
   begin
      R := R2;
      R2 := 0;
   end P2;

end Function_Style_Procedures;
