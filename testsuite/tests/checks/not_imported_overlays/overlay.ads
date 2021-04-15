package Overlay is
   I : Integer;

   J : Integer with Address => I'Address;  --  FLAG
   K : Integer;
   for K'Address use I'Address;            --  FLAG

   L : Integer;
   for L'Address use I'Address;            --  NO FLAG
   pragma Import (C, L);

   M : Integer with Import;
   for M'Address use I'Address;            --  NO FLAG
end Overlay;
