procedure Op is
   type Enum is range 1 .. 3;  --  FLAG
   type Int is range 1 .. 3;   --  NO FLAG

   X : Enum := 1;
   Y : Int := 1;

begin
   X := 2;
   Y := Y + 1;
end Op;
