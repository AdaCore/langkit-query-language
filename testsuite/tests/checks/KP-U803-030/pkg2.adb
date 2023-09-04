package body Pkg2 is
   function Func (S : String) return String is ("abc");

begin
   pragma Assert (Func ("def") = "abcdef");  --  NOFLAG
end Pkg2;
