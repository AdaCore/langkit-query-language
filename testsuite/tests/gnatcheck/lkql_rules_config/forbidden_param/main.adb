procedure Main is
   pragma Ada_2022;  --  FLAG (2)
   pragma Custom ("This is my custom pragma");  --  FLAG

   procedure Test is null;
   pragma Annotate (T, Id);  --  NOFLAG
begin
   pragma Assert (False);  --  FLAG
end Main;
