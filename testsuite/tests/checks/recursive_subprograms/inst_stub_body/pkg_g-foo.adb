separate (Pkg_G)
procedure Foo is
   procedure Rec is  --  FLAG
   begin
      Rec;
   end Rec;
begin
   Rec;
end Foo;
