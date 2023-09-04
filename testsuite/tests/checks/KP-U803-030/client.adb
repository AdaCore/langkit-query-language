with Pkg;
procedure Client is
   pragma Assertion_Policy (Check);
begin
   pragma Assert (Pkg.Prepend_Abc ("def") = "abcdef");   --  FLAG
   pragma Assert (Pkg2.Prepend_Abc ("def") = "abcdef");  --  NOFLAG
end Client;
