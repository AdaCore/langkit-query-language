procedure Paths is
   type Enum is (A, B);
   Obj : Enum;
begin
   case Obj is  --  NOFLAG
      when A => null;
      when B => null;
   end case;

   case True is  --  NOFLAG
      when True => null;
      when others => null;
   end case;

   case 1 is  -- FLAG
      when 1 => null;
      when others => null;
   end case;
end Paths;
