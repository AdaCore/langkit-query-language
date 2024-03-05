procedure Main is
begin
   goto Test;  --  FLAG (2)

   <<Test>>

   if True then
      goto The_End;  --  FLAG
   end if;

   <<The_End>>
end Main;
