procedure Ident is
   I : Integer;              --  NO FLAG
   J : String (1 .. 10);     --  FLAG

   type T is null record;    --  FLAG

   procedure P with Import;  --  FLAG

begin
   null;
end Ident;
