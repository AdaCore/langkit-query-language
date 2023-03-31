procedure Same is

   function Same_Op (X : Natural) return Integer is
      Y : Integer;
   begin
      Y := (X + 1) / (X - 1);        --  NO FLAG
      return (X + 1) / (x +  1);     --  FLAG
   end Same_Op;

   function Is_Nan (F : Float) return Boolean is (F = F);  --  NO FLAG

   F      : Float;
   Is_Inf : Boolean := Is_NaN (F - F);     --  NO FLAG

   G  : constant := 2 ** (31 - 15);        --  NO FLAG

begin
   null;
end Same;
