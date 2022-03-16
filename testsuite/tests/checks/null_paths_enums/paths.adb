procedure Paths is
   type Enum is (A, B);
   Obj : Enum;
begin
   case Obj is
      when A =>
         null;    --  NO FLAG
      when B =>
         null;    --  NO FLAG
   end case;
end Paths;
