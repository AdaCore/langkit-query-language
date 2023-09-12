package Test
is
   procedure P; -- FLAG

   procedure Q is null; -- NOFLAG

   function Foo return Integer  -- FLAG
   is (12)
   with SPARK_Mode => On;

   V : Integer;

   procedure T with Global => V;  -- NOFLAG
end Test;
