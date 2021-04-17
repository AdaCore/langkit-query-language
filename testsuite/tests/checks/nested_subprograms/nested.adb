procedure Nested (I, J : in out Integer) is

   procedure Foo (K : Integer) is null;
   procedure Proc1;                    --  FLAG

   procedure Proc2 is separate;        --  FLAG

   procedure Proc1 is
   begin
      I := I + J;
   end Proc1;

begin
   null;
end Nested;
