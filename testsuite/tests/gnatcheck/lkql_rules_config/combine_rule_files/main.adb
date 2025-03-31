procedure Main is
begin
   null;
   goto First;  --  FLAG

   <<First>>

   if C then
      goto Second;  --  FLAG
   end if;

   <<Second>>
end Main;
