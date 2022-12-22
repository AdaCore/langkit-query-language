package Params is
   type Rec is record
      Field1 : Integer;
   end record;

   type Rec2 is record
      F : Rec;
   end record;

   procedure Proc (P1 : Rec; P2 : out Rec);
end Params;
