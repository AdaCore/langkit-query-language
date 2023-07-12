procedure Same is

   function Same_Op (X : Natural) return Integer is
      Y : Integer;
   begin
      Y := (X + 1) / (X - 1);        -- NOFLAG
      return (X + 1) / (x +  1);     --  FLAG
   end Same_Op;

   function Is_Nan (F : Float) return Boolean is (F = F);  -- NOFLAG

   F      : Float;
   Is_Inf : Boolean := Is_NaN (F - F);     -- NOFLAG

   G  : constant := 2 ** (31 - 15);        -- NOFLAG

begin
   null;
end Same;
