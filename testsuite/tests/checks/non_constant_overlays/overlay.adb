procedure Overlay (I : Integer) is

   V : Integer with Volatile;
   C : constant Integer := 1;
   for C'Address use V'Address;    --  FLAG

   C : Integer := 1;
   V : constant Integer;
   for V'Address use C'Address;           --  FLAG: C is not Volatile

begin
   null;
end Overlay;
