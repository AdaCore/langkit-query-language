package Expr_P is
   function G return Boolean;          --  FLAG

private
   function F return Boolean is (G);   --  FLAG
end Expr_P;
