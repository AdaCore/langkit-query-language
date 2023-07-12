package P is

   type Unit is digits 6;
   subtype F is Unit;
   type Dimension is (aa, bb);
   type Coordinates is array (Dimension) of F;
   subtype Coordinates2 is Coordinates;
   type Coordinates3 is array (1 .. 2) of F;
   subtype Index is Integer range 2 .. 3;
   type Coordinates4 is array (Index) of F;

   type Coordinates5 is array (2 .. 4) of F;

   function Func return Coordinates;   --  FLAG
   function Func return Coordinates2;  --  FLAG
   function Func return Coordinates3;  --  FLAG
   function Func return Coordinates4;  --  FLAG

   function Func return Coordinates5;  -- NOFLAG

end P;
