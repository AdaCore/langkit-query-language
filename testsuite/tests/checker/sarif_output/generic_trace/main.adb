procedure Main is
   generic
      type T is private;
   procedure Inner (V : T);

   procedure Inner (V : T) is
   begin
      goto x;
      <<x>>
   end Inner;

   generic
      type T is private;
   procedure Outer (V : T);

   procedure Outer (V : T) is
      procedure Inner_T is new Inner (T);
   begin
      null;
   end Outer;

   procedure Outer_T is new Outer (Integer);
begin
   null;
end Main;
