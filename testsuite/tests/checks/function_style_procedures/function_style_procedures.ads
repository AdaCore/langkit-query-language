package Function_Style_Procedures is

   procedure P (R : out Integer)       --  FLAG
     with Global => null;

   type Lim is limited null record;
   procedure P_Limited (R : out Lim);  -- NOFLAG

   type Non_Limited_Rec is null record;
   procedure P_Non_Limited_Rec (R : out Non_Limited_Rec);  -- FLAG

   procedure P_Null (R : out Integer) is null;  -- NOFLAG

   procedure P2 (R : out Integer; R2: in out Integer);  -- NOFLAG

   Global_Var : Integer := 1;

   procedure Glob (R : out Integer)   -- NOFLAG
     with Global => Global_Var;

end Function_Style_Procedures;
