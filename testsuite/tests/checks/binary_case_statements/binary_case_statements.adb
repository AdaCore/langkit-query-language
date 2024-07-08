procedure Main is
   procedure Test (I : Integer);
begin
   case 1 is  --  FLAG
      when 1 => null;
      when others => null;
   end case;

   case 1 is  --  FLAG
      when 1 => Test (2);
      when 2 => Test (1);
   end case;

   case True is  --  FLAG
      when True => null;
      when False => null;
   end case;

   case 1 is  --  NOFLAG
      when 1 | 2 => null;
      when others => null;
   end case;

   case 1 is  --  NOFLAG
      when 1 => null;
      when 2 => null;
      when others => null;
   end case;
end Main;
