package Pack is
   type UInt8 is mod 2**8;

   MTU : constant := 64 * 1024 + 48;

   subtype Data_Index is Natural range 0 .. MTU - 1;
   type Data_Type is array (Data_Index) of UInt8;

   procedure Call  (Data : out Data_Type);
end Pack;
