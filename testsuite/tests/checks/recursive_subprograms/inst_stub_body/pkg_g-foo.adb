separate (Pkg_G)
procedure Foo is
   procedure Rec is
   begin
      Rec;
   end Rec;
begin
   Rec;
end Foo;
