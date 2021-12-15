procedure Op is
   type Enum is range 1 .. 3;    --  FLAG
   type Int is range 1 .. 3;     --  NO FLAG

   type Enum2 is range 1 .. 10;  --  NO FLAG
   type Enum_D is new Enum2;

   X : Enum := 1;
   Y : Int := 1;
   Z : Enum_D := 1;

begin
   X := 2;
   Y := Y + 1;
   Z := Z + 1;
end Op;
