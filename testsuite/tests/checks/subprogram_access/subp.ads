package Subp is
   type Proc_A is access procedure ( I : Integer);       --  FLAG

   procedure Proc
     (I       : Integer;
      Process : access procedure (J : in out Integer));  --  FLAG
end Subp;
