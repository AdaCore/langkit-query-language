package Function_Style_Procedures is

   procedure P (R : out Integer);  -- FLAG

   type Lim is limited null record;
   procedure P_Limited (R : out Lim);  -- NO FLAG

   procedure P_Null (R : out Integer) is null;  -- NO FLAG
   
   procedure P2 (R : out Integer; R2: in out Integer);  -- NO FLAG

end Function_Style_Procedures;
