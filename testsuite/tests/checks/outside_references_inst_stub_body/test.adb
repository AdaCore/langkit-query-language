with Pkg_G;

procedure Test is
   package My_Pkg is new Pkg_G;
begin
   My_Pkg.Foo;
end Test;
