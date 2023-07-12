separate (Pkg_G)
procedure Foo is
   X : Integer;

   procedure Bar is
   begin
      X := 1;  --  FLAG
   end Bar;
begin
   Bar;
end Foo;
