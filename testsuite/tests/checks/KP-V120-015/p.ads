package P is

   type Enum1 is (Val1, Val2);

   type Enum2 is (A, B, C, D);
   for Enum2'Size use 12;

   type Enum3 is (E, F, G, H);
   for Enum3'Size use 12;

   type Union (Disc : Enum1 := Val1) is record
      case Disc is
         when Val1 => Comp1 : Enum2;
         when Val2 => Comp2 : Enum3;
      end case;
   end record;
   pragma Unchecked_Union (Union);

   for Union use record
      Comp1 at 0 range 0 .. 11;
      Comp2 at 0 range 0 .. 11;
   end record;
   for Union'Size use 12;

   type Rec is record
      Field1 : Union;    --  FLAG
      Field2 : Integer;  --  NO FLAG
   end record;
   
end;
