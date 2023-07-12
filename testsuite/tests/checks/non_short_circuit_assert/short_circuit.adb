procedure Short_Circuit (I, J : Integer) is

   type Mod_8 is mod 2**8;

   function F1 (I, J : Mod_8) return Mod_8
     with Pre => I > 1 and J < 1  -- NOFLAG: assertion
   is
   begin
      return "AND" (I, J);        -- NOFLAG
   end F1;

   B1 : Boolean;
begin
   B1 := I > 0 and J > 0;            --  FLAG
   pragma Assert (I > J or J > 0);   -- NOFLAG: assertion
end Short_Circuit;
