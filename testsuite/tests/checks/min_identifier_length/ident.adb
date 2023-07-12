procedure Ident is
   I : Integer;              -- NOFLAG
   J : String (1 .. 10);     --  FLAG

   type T is null record;    --  FLAG

   procedure P with Import;  --  FLAG

begin
   null;
end Ident;
