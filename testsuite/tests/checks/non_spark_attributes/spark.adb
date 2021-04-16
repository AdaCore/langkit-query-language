procedure SPARK is
   type Integer_A is access all Integer;

   Var : aliased Integer := 1;
   Var_A : Integer_A := Var'Access;  --  FLAG
begin
   null;
end SPARK;
