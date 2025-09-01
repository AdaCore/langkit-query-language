procedure Generics is
   generic
      type T is range <>;      -- NOFLAG
   package Pkg is
      -- T is never used as an integer type so in theory it could be flagged
      -- by this rule. However, we don't want to flag formal types.
      type U is range 1 .. 3;  -- FLAG
      -- make sure that concrete type declarations inside generics are still
      -- flagged though.
   end Pkg;

   package My_Pkg is new Pkg (Integer);
begin
   null;
end Generics;
