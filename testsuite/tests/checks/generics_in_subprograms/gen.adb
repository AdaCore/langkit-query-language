procedure Gen is

   generic
      type FT is range <>;
   function F_G (I : FT) return FT;   --  FLAG

   function F_G (I : FT) return FT is (I);

begin
   null;
end Gen;
