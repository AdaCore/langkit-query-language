procedure Bit is
   type Enum is mod 2 ** 8;       --  FLAG
   type Modular is mod 2 ** 8;    --  NO FLAG

   X : Enum := 1;
   Y : Modular := 1;

begin
   Y := Y or 2;
end Bit;
