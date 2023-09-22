package body P is

   function Work return Discr is
     (Discr'(Content    => Yellow,
             Data       => 1));

   function Work (X : Integer) return Discr is
   begin
      return D : Discr do
         null;
      end return;
   end;

   function Work return Integer is
      D  : constant Discr := Work;      --  FLAG
      D2 : constant Discr := Work (1);  --  FLAG
   begin
      return 0;
   end Work;

end P;
