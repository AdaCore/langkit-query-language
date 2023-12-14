procedure Loop2 is
   type Arr_T is array (Positive range <>) of Integer;
   type Rec_T is record
      Bounds : Arr_T (1 .. 3);
   end record;

   X, Y : Rec_T := (Bounds => (others => 0));
begin
   for I in X.Bounds'Range loop                -- NOFLAG
      X.Bounds (I) := Y.Bounds (I);
   end loop;

   for I in X.Bounds'Range loop                -- FLAG
      X.Bounds (I) := X.Bounds (I);
   end loop;
end Loop2;
