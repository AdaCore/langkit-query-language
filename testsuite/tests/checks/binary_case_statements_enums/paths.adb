procedure Paths is
   type Enum is (A, B);
   Obj : Enum;
begin
   case Obj is   -- NOFLAG
      when A =>
         null;
      when B =>
         null;
   end case;
end Paths;
