procedure Main is
   I : Integer := 0;
begin
   if I'Finalization_Size > 0 then null; end if;  --  FLAG
end Main;
