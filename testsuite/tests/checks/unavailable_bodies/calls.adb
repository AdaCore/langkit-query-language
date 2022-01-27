procedure Calls is

   procedure Unknown with Import;

   function F return Integer is (1);

   Val : Integer := F;      --  NO FLAG

   type Proc_A is access procedure (X : Integer);
   X : Proc_A;

   type Proc_B is access procedure;
   Y : Proc_B;

   generic
      with procedure P_F;
   package P_G is
      X : Integer;
      procedure P;
   end P_G;

   package body P_G is
      procedure P is
      begin
         P_F;   --  NO FLAG
      end P;
   end P_G;

begin
   Unknown;     --  FLAG
   X.all (1);   --  FLAG
   X (1);       --  FLAG
   Y.all;       --  FLAG
end Calls;
