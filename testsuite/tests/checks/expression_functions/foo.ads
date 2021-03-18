package Foo is
   function F (I : Integer) return Integer is   --  FLAG
     (if I > 0 then I - 1 else I + 1);
end Foo;
