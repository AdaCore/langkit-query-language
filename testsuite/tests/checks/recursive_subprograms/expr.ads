package Expr is
   function F (I : Integer) return Integer is   --  NO FLAG
     (if I > 0 then I - 1 else I + 1);

private
   function F2 return Boolean is  --  NO FLAG
     (True);
end Expr;
