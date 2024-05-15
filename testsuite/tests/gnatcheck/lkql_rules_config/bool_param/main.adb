procedure Main is
   C : constant Boolean := False and True;  --  FLAG
   pragma Assert (True or True);  --  FLAG
begin
   goto First;  --  FLAG

   <<First>>

   if C then
      goto Second;  --  NOFLAG
   end if;

   <<Second>>
end Main;
