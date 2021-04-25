with Inlined; use Inlined;

package body Deeply_Inlined is

   procedure P3 is
   begin
      P2;
   end P3;

   procedure P4 is  --  FLAG
   begin
      P3;
   end P4;
   pragma Inline (P4);

   procedure P5 is
   begin
      P4;
   end P5;

   procedure P6 is
   begin
      P5;
   end P6;

   procedure P7 is
   begin
      P6;
   end P7;

   function F3 return Boolean is
   begin
      return F2;
   end F3;

   function F4 return Boolean is
   begin
      return F3;
   end F4;

   function F5 return Boolean is  --  FLAG
   begin
      return F4;
   end F5;
   pragma Inline (F5);

   function F6 return Boolean is
   begin
      return F5;
   end F6;

   --  F7 is not an inlined subprogram
   function F7 return Boolean is
   begin
      return F6;
   end F7;

   function F8 return Boolean is
   begin
      return F7;
   end F8;

   function F9 return Boolean is
   begin
      return F6;
   end F9;

   function F10 return Boolean is
      Result : Boolean;
   begin
      Result := F8;
      Result := F7;
      return F6;
   end F10;

end Deeply_Inlined;
