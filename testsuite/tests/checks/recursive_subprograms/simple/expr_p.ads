package Expr_P is
   function G return Boolean;          --  FLAG

private
   function F return Boolean is (G);   --  FLAG
   function F2 return Boolean is       -- NOFLAG
     (True);
end Expr_P;
