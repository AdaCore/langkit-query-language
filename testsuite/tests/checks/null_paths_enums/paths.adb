procedure Paths is
   type Enum is (A, B);
   Obj : Enum;
begin
   case Obj is
      when A =>
         null;    --  NOFLAG
      when B =>
         null;    --  NOFLAG
   end case;
end Paths;
