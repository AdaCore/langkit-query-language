package body Inlined is

   procedure P0 is
   begin
      null;
   end P0;

   procedure P1 is
   begin
      P0;
   end P1;

   procedure P2 is
   begin
      P1;
   end P2;

   function F0 return Boolean is
   begin
      return False;
   end F0;

   function F1 return Boolean is
   begin
      return F0;
   end F1;

   function F2 return Boolean is
   begin
      return F1;
   end F2;

end Inlined;
