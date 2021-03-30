procedure Expr is

   subtype Ind is Integer range 1 .. 10;
   type Matrix is array (Ind, Ind) of Integer;

   function Check_Matrix (M : Matrix) return Boolean is
     (for some I in Ind =>                               --  FLAG
        (for all J in Ind => M (I, J) = 0));             --  FLAG

begin
   null;
end Expr;
