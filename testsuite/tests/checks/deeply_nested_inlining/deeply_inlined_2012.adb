package body Deeply_Inlined_2012 is

   procedure P0 is
   begin
      null;
   end P0;

   procedure New_P0 is new P0;   -- NO FLAG
   procedure P1 is
   begin
      New_P0;
   end P1;

   procedure New_P1 is new P1;   -- NO FLAG
   procedure P2 is
   begin
      New_P1;
   end P2;

   procedure New_P2 is new P2;   -- NO FLAG
   procedure P3 is
   begin
      New_P2;
   end P3;

   procedure New_P3 is new P3;   -- NO FLAG
   procedure P4 is
   begin
      New_P3;
   end P4;

   procedure New_P4 is new P4;   -- FLAG
   procedure P5 is
   begin
      New_P4;
   end P5;

end Deeply_Inlined_2012;
