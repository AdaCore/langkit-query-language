with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar;  --  FLAG

procedure Main is
   task T is
      entry X;  --  FLAG
      entry Y;
      entry Z;
   end T;

   task body T is
   begin
      null;
   end T;

   function Test return Boolean is (True);
   X : access function return Boolean := Test'Access;  --  FLAG
begin
   Put_Line ("Hello!");  --  FLAG
end Main;
