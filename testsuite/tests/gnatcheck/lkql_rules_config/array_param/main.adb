procedure Main is
   package Test is
      V : Integer := 50;
   end Test;

   B : Boolean := True;
begin
   if B = True             --  FLAG
     and then Test.V = 50  --  FLAG
   then
      null;
   end if;
end Main;
