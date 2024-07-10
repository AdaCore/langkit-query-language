package Pack is
   B : Boolean;                  --  FLAG
   I1 : Integer;                 --  FLAG
   C : Character;                --  FLAG
   W_C : Wide_Character;         --  FLAG
   W_W_C : Wide_Wide_Character;  --  FLAG
   I2 : Integer := 13;           --  NOFLAG

   generic
      type T is private;
      ZZZ : T;            --  NOFLAG
   package Pack_G is
      X : T;              --  FLAG
      Y : T := ZZZ;       --  NOFLAG
   end Pack_G;

   C1 : constant Integer := 1;  --  NOFLAG
   C2 : constant Integer;       --  NOFLAG

private
   Y1 : Integer;                --  FLAG
   Y2 : Integer := 12;          --  NOFLAG
   C2 : constant Integer := 1;  --  NOFLAG
end Pack;
