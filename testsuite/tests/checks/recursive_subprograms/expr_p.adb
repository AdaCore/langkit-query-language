package body Expr_P is

   function Expr return Boolean is  --  FLAG
   begin
      return F;
   end;

   function G return Boolean is
   begin
      return Expr;
   end G;

end Expr_P;
