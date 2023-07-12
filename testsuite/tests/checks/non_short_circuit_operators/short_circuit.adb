procedure Short_Circuit (I, J : Integer) is

   type Mod_8 is mod 2**8;

   function F1 (I, J : Mod_8) return Mod_8 is
   begin
      return "AND" (I, J);      -- NOFLAG
   end F1;

   B1 : Boolean;
begin
   B1 := I > 0 and J > 0;       --  FLAG
   B1 := I < 0 and then J < 0;  -- NOFLAG
   B1 := I > J or J > 0;        --  FLAG
   B1 := I < J or else I < 0;   -- NOFLAG
   B1 := "AND" (I < J, I < 0);  --  FLAG
end Short_Circuit;
