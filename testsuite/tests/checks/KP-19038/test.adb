with Ada.Text_IO;

procedure Test is
   protected type PT with Exclusive_Functions is   -- FLAG
      function Unlock return Boolean;
      procedure Unlock;
      entry E;
   end PT;

   Locked : Boolean := True;

   protected body PT is
      function Unlock return Boolean is
         Previous : constant Boolean := Locked;
      begin
         Locked := False;
         return Previous;
      end;

      procedure Unlock is
      begin
         Locked := False;
      end;

      entry E when not Locked is
      begin
         Locked := True;
      end;
   end;

   PO : PT;

   task T;

   task body T is
      Unused : Boolean;
   begin
      delay 1.0;
      PO.Unlock;
      delay 1.0;
      Unused := PO.Unlock;
   end;

begin
   select
      PO.E;
      Ada.Text_IO.Put_Line ("Entry call succeeded");
   or
      delay 2.0;
      Ada.Text_IO.Put_Line ("Entry call timed out");
   end select;

   select
      PO.E;
      Ada.Text_IO.Put_Line ("Entry call succeeded");
   or
      delay 2.0;
      Ada.Text_IO.Put_Line ("Entry call timed out");
   end select;
end Test;
