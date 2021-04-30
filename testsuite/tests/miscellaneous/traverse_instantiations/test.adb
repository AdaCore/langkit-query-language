procedure Test is
   generic
      type T is private;
   procedure Foo;

   procedure Foo is
      Inst : T;
   begin
      null;
   end Foo;


   generic
      type T is private;
   package Bar is
      Inst : T;
   end Bar;

   A : Integer;


   procedure Int_Foo is new Foo (Integer);

   package IntBar is new Bar (Integer);
begin
   null;
end Test;
