procedure Main is
   function Add (X, Y : Integer) return Integer
   is (X + Y);

   I : Integer;
begin
   I := Add (1, 2);
end Main;
