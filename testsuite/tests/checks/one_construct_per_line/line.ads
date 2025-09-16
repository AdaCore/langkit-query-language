package Line is I : Integer;                      --  FLAG
   B : Boolean;                                   --  NOFLAG
   F1 : Float;                                    --  NOFLAG
   F2 : Float; C :                                --  FLAG (2)
   Character;

   generic
      type Data_Type is private;
   package Pkg_A is
   end Pkg_A;

   generic
      with package Data_Pkg is new Pkg_A (<>);  --  NOFLAG
      with procedure Test (X : Integer) is <>;  --  NOFLAG
   package Pkg_B is
   end Pkg_B;

   type T (A : Integer; B : Integer) is null record;  --  NOFLAG

   procedure Proc (I : in out Integer); end Line; --  FLAG
