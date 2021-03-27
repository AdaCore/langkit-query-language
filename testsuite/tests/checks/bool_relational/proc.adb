procedure Proc (Flag_1 : Boolean; Flag_2 : Boolean; I : in out Integer) is
   function "<=" (A, B : Boolean) return Boolean with Import;
   A : Boolean;

begin
   if Flag_1 >= Flag_2 then     --  FLAG
      null;
   end if;

   if Flag_1 <= Flag_2 then     --  NO FLAG: user defined
      null;
   end if;

   if Boolean'Pos (Flag_1) < Boolean'Pos (Flag_2) then    --  NO FLAG
      null;
   end if;

   A := (if "<" (Flag_1, Flag_2) then   --  FLAG
           (case I is
              when 1 =>
                 Flag_1 > Flag_2,       --  FLAG
              when  others =>
                 Flag_1 = Flag_2));     --  FLAG
end Proc;
