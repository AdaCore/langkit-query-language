package Max is

   procedure Proc_1 (I : in out Integer);          --  NO FLAG
   procedure Proc_2 (I, J : in out Integer);       --  NO FLAG
   procedure Proc_3 (I, J, K : in out Integer);    --  NO FLAG
   procedure Proc_4 (I, J, K, L : in out Integer); --  FLAG (if n is 3)

   function Fun_4                                  --  FLAG (if n is 3)
     (I : Integer;
      J : Integer;
      K : Integer;
      L : Integer) return Integer is (I + J * K - L);

end Max;
