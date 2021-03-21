package Pack is
   I1 : Integer;          --  FLAG
   I2 : Integer := 13;    --  NO FLAG

   generic
      type T is private;
      ZZZ : T;            --  NO FLAG
   package Pack_G is
      X : T;              --  FLAG
      Y : T := ZZZ;       --  NO FLAG
   end Pack_G;

   C1 : constant Integer := 1;  --  NO FLAG
   C2 : constant Integer;       --  NO FLAG

private
   Y1 : Integer;                --  FLAG
   Y2 : Integer := 12;          --  NO FLAG
   C2 : constant Integer := 1;  --  NO FLAG
end Pack;
