package Q is   --  NO FLAG
   type T is tagged null record;

   package Nested is
      type T is tagged null record;
   end Nested;

end Q;
