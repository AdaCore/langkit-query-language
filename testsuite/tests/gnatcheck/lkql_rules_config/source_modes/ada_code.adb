procedure Ada_Code is
   type Int   is range 0 .. 100;  --  FLAG
   type Int_T is range 0 .. 100;  --  NOFLAG
begin
   Decl := @ + 12;  --  NOFLAG because not in SPARK mode

   <<Infinite>>
   Put_Line("Hello world!");
   goto Infinite; --  FLAG
end Ada_Code;
