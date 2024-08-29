procedure Transitive is
   procedure Foo;  --  FLAG
   procedure Bar;  --  FLAG
   procedure Baz;  --  FLAG

   procedure Foo is
   begin
      Bar;
   end Foo;

   procedure Bar is
   begin
      Baz;
   end Bar;

   procedure Baz is
   begin
      Foo;
   end Baz;
begin
   null;
end Transitive;
