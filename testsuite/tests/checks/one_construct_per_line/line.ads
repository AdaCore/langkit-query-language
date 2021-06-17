package Line is I : Integer;                      --  FLAG
   B : Boolean;                                   --  NO FLAG
   F1 : Float;                                    --  NO FLAG
   F2 : Float; C :                                --  FLAG
   Character;

   type T (A : Integer; B : Integer) is null record;  --  NO FLAG

   procedure Proc (I : in out Integer); end Line; --  FLAG
