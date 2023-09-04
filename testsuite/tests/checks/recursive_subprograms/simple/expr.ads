package Expr is
   function F (I : Integer) return Integer is   --  NOFLAG
     (if I > 0 then I - 1 else I + 1);

private
   function F2 return Boolean is  --  NOFLAG
     (True);
end Expr;
