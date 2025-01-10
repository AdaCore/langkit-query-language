procedure Main (Arg : Integer) is

   package Container is
      type Arr is array (Natural range <>) of Float;

      type Vector (X : Integer) is tagged record
         Content : Arr (1 .. X);
      end record
      with Constant_Indexing => Constant_Reference;

      function Constant_Reference (V : Vector; Pos : Integer) return Float
      is (V.Content (Pos));
   end Container;

   subtype S1 is Container.Vector (Arg);
   subtype S2 is Container.Vector (3);

   V : S1;

   function Ident1 (X : S1) return S1
   is (X);

   W : S2;

   function Ident2 (X : S2) return S2
   is (X);

   Res : Float;

begin
   V := (Arg, (others => 1.0));
   Res := Ident1 (V) (4);  -- FLAG

   W := (3, (1.0, 2.0, 3.0));
   Res := Ident2 (W) (4);  -- NO FLAG
end Main;
