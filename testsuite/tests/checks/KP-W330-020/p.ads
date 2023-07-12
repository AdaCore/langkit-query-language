with System;

package P is

  Data : constant Small_Unsigned := 42;
  for Data'Size use 8;

  Data2, Data3 : Integer;
  for Data2'Size use 8;

  A, B : Integer;

  X : System.Address := Data'Address;   -- FLAG
  Y : System.Address := A'Address;      -- NOFLAG
  Z : System.Address := Data2'Address;  -- FLAG

end P;
