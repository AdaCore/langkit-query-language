package body P is

   procedure Proc (Object : One'Class) is
      S : constant String := Object'Image;  --  FLAG
   begin
      null;
   end Proc;

end P;
