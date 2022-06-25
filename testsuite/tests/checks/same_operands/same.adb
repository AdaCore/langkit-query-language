procedure Same is

   function Same_Op (X : Natural) return Integer is
      Y : Integer;
   begin
      Y := (X + 1) / (X - 1);        --  NO FLAG
      return (X + 1) / (x +  1);     --  FLAG
   end Same_Op;

   F      : Float;
   Is_Nan : Boolean := F = F;     --  NO FLAG

begin
   null;
end Same;
