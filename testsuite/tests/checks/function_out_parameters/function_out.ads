package Function_Out is
   function F_1 (I : Integer) return Integer;            --  NOFLAG
   function F_2 (I : in Integer) return Integer;         --  NOFLAG
   function F_3 (I : out Integer) return Integer;        --  FLAG
   function F_4 (I : in out Integer) return Integer;     --  FLAG

   function Expr_F (I : out Integer) return Integer is   --  FLAG
     (I);

   generic
      type T is private;
   function Gen_F (I : out T) return Integer;            --  FLAG

   procedure P (I : out Integer);                        --  NOFLAG
end Function_Out;
