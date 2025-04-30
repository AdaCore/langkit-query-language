procedure Small is
   type T1 is delta 0.01 range -1.0 .. 1.0;  -- FLAG
   type T2 is delta 0.01 range -1.0 .. 1.0;  -- NOFLAG

   generic
     type T3 is delta <>;                   -- NOFLAG
   package G is
   end G;

   for T2'Small use 0.01;
begin
   null;
end Small;
