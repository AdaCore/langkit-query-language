package P is

   type T is tagged null record;
   function F (O : T) return Integer is (0);   --  FLAG

   type T2 is new T with private;

   package Pkg1 is
      type T1 is tagged null record;
      function Prim (X1 : T1) return Integer;
   end Pkg1;

   package Pkg2 is
      type Priv is private;
      type Rec is record Aaa, Bbb : Priv; end record;

      type T2 is new Pkg1.T1 with null record;

      overriding function Prim (X2 : T2) return Integer is (Rec'Size);  --  FLAG

      Obj : Pkg1.T1'Class    := T2'(null record);
      Sz  : constant Integer := Pkg1.Prim (Obj);
   private
      --  Priv'Size depends on Priv'Last, which depends on Rec'Size,
      --  which depends on Priv'Size. This cyclic dependency demonstrates
      --  that this example *should* be illegal. This language definition
      --  oversight is corrected in AI22-0042.
      type Priv is new String (1 .. Sz);
   end Pkg2;

private
   type T2 is new T with null record;
   function F (O : T2) return Integer is (1);  --  FLAG
end P;
