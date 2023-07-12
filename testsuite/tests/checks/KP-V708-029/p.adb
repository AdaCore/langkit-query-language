procedure P is
   X : String (1 .. 3) := (Character'Val (96),
                           Character'Val (220),
                           Character'Val (255));

   Int : Integer := 0;
   S1  : String := X'Image;   --  FLAG
begin
   S1 := String'Image (X);   --  FLAG
   S1 := Int'Image;          -- NOFLAG
end P;
