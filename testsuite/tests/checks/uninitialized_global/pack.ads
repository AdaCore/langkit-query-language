package Pack is
   I1 : Integer;          --  FLAG
   I2 : Integer := 13;    -- NOFLAG

   generic
      type T is private;
      ZZZ : T;            -- NOFLAG
   package Pack_G is
      X : T;              --  FLAG
      Y : T := ZZZ;       -- NOFLAG
   end Pack_G;

   C1 : constant Integer := 1;  -- NOFLAG
   C2 : constant Integer;       -- NOFLAG

private
   Y1 : Integer;                --  FLAG
   Y2 : Integer := 12;          -- NOFLAG
   C2 : constant Integer := 1;  -- NOFLAG
end Pack;
