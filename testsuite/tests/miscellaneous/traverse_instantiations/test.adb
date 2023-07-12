procedure Test is
   generic
      type T is private;
   procedure Foo;

   procedure Foo is
      Inst : T;  --  FLAG
   begin
      null;
   end Foo;


   generic
      type T is private;
   package Bar is
      Inst : T;  --  FLAG
   end Bar;

   A : Integer;  --  FLAG


   procedure Int_Foo is new Foo (Integer);

   package IntBar is new Bar (Integer);
begin
   null;
end Test;
