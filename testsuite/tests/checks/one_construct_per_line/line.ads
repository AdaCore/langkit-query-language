package Line is I : Integer;                      --  FLAG
   B : Boolean;                                   --  NOFLAG
   F1 : Float;                                    --  NOFLAG
   F2 : Float; C :                                --  FLAG (2)
   Character;

   type T (A : Integer; B : Integer) is null record;  --  NOFLAG

   procedure Proc (I : in out Integer); end Line; --  FLAG
