with Ada.Unchecked_Conversion;

package body Pack is
   Device_Size : constant := 312;

   type Device_Type is record
      Fnord : UInt8;
   end record;

   for Device_Type'Size use Device_Size * 8;

   type Device_Packet is record
      Device : Device_Type;
   end record;

   for Device_Packet use record
      Device at 0 range 0 .. (Device_Size * 8) - 1;
   end record;

   for Device_Packet'Size use MTU * 8;

   function Convert is new Ada.Unchecked_Conversion
     (Source => Device_Packet,
      Target => Data_Type);

   procedure Call (Data : out Data_Type) is
   begin
      Data := Convert   --  FLAG
                (S => Device_Packet'(Device => Device_Type'(Fnord => 1)));
   end Call;

end Pack;
