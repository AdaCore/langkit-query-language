procedure Main is
begin
   if True then
      declare  --  FLAG
         X : Integer := 42;
         Y : Integer := 42;
      begin
         null;
      end;
   else
      declare
         X : Integer := 42;
         Y : Integer := 42;
      begin
         null;
      end;
   end if;
end Main;
