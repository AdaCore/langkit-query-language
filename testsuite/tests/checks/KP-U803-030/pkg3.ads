package Pkg is
   function Prepend_Abc (S : String) return String is ("abc" & S);

   pragma Assert (Prepend_Abc ("def") = "abcdef");  --  NO FLAG
end Pkg;
