procedure Fixed_Equality_Checks is
   type Volt is delta 0.125 range 0.0 .. 255.0;
   A_Volt, B_Volt : Volt := 0.0;

   B1 : constant Boolean := A_Volt = B_Volt;    -- FLAG
   B2 : constant Boolean := A_Volt /= B_Volt;   -- FLAG

   Result : Boolean;

begin
   Result := "=" (A_Volt, B_Volt);              -- FLAG
   Result := Fixed_Equality_Checks."/=" (A_Volt, B_Volt);   -- FLAG
end Fixed_Equality_Checks;
