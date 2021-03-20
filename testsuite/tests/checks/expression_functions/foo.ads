package Foo is
   function F (I : Integer) return Integer is   --  FLAG
     (if I > 0 then I - 1 else I + 1);

private
   function F2 return Boolean is  --  FLAG
     (True);
end Foo;
