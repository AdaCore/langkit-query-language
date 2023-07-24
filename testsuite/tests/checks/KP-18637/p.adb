package body P is

   procedure Check (Object : Som'Class) is
      P : Parent'Class := Parent'Class (Object);  --  FLAG
   begin
      null;
   end Check;

end P;
