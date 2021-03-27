procedure Cond (I, J : Integer) is
   Var1 : Integer := (if I > J then 1 else 0);  --  FLAG
   Var2 : Integer := I + J;

   function F (B : Boolean) return Boolean is (B)
     with Pre'Class => (if B then False); --  FLAG unless Except_Assertions

begin
   pragma Assert (if I = 0 then False);   --  FLAG unless Except_Assertions
end Cond;
