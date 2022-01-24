procedure Calls is

   procedure Unknown with Import;

   function X return Integer is (1);

   Val : Integer := X;      --  NO FLAG

   type Proc_A is access procedure (X : Integer);
   X : Proc_A;

   type Proc_B is access procedure;
   Y : Proc_B;

begin
   Unknown;     --  FLAG
   X.all (1);   --  FLAG
   X (1);       --  FLAG
   Y.all;       --  FLAG
end Calls;
