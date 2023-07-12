package Q is   -- NOFLAG
   type T is tagged null record;

   package Nested is
      type T is tagged null record;
   end Nested;

end Q;
