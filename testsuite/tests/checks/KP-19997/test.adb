with Ada.Unchecked_Conversion;

with Interfaces;  use Interfaces;
with System;      use System;

procedure Test is
   type Bytes is array (0 .. 3) of Unsigned_8
     with Scalar_Storage_Order => Low_Order_First;

   type Bytes_BE is new Bytes
     with Scalar_Storage_Order => High_Order_First;

   type Bytes_LE is new Bytes
     with Scalar_Storage_Order => Low_Order_First;

   function To_Bytes is new Ada.Unchecked_Conversion (Unsigned_32, Bytes);  -- NOFLAG

   function To_Bytes_BE is new Ada.Unchecked_Conversion (Unsigned_32, Bytes_BE);  --  FLAG

   function To_Bytes_LE is new Ada.Unchecked_Conversion (Unsigned_32, Bytes_LE);  --  NOFLAG
begin
   null;
end;
