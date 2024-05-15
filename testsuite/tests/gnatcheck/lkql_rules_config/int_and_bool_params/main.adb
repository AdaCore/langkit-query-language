procedure Main is
   I : constant Integer := 10;  --  NOFLAG
begin
   if I < 10 then    --  FLAG
      if I > 2 then  --  NOFLAG
      end if;
   end if;

   case True is
      when False => null;
      when others => null;  --  FLAG
   end case;

   --## rule off numeric_literals ## We don't want to flag this
   if 1 in 1 .. 2 then        --  NOFLAG
      null;
   elsif 1.5 in 1.5 .. 2.0 then  --  FLAG
      null;
   end if;
   --## rule on numeric_literals
end Main;
