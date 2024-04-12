procedure Ada_Code is
   type Int   is range 0 .. 100; --  FLAG
   type Int_T is range 0 .. 100;

   type Int_A   is access Int; --  FLAG
   type Int_PTR is access Int; --  FLAG

   Const   : constant Int := 1; --  FLAG
   Const_C : constant Int := 1;

   Decl : Integer := 10;

   X : array (1 .. 10) of Integer; --  FLAG
begin
   Decl := @ + 12;  --  NOFLAG because not in SPARK mode

   <<Infinite>>
   Put_Line("Hello world!");
   goto Infinite; --  FLAG
end Ada_Code;
