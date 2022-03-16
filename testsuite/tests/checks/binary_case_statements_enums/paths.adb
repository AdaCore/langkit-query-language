procedure Paths is
   type Enum is (A, B);
   Obj : Enum;
begin
   case Obj is   --  NO FLAG
      when A =>
         null;
      when B =>
         null;
   end case;
end Paths;
