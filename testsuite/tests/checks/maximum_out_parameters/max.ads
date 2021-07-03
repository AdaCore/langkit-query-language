package Max is

   procedure Proc_1 (I : in out Integer);          --  NO FLAG
   procedure Proc_2 (I, J : in out Integer);       --  NO FLAG
   procedure Proc_3 (I, J, K : in out Integer);    --  NO FLAG
   procedure Proc_4 (I, J, K, L : in out Integer); --  FLAG (if n is 3)
   procedure Proc_5 (I, J, K : Integer; L : out Integer); --  NO FLAG

end Max;
