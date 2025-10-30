package body With_Use_VADS is
   procedure Test (X, Y : Integer) is
      type Local_Arr is array (Integer range <>) of Integer;
      subtype Local_Sub is Local_Arr (X .. Y);

      Local_Sub_Size : Integer := Local_Sub'Size;  --  FLAG
   begin
      null;
   end Test;
end With_Use_VADS;
