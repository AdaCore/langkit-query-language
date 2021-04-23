procedure Ret is

   procedure Proc (I : in out Integer) is
   begin
      if I = 0 then
         return;                          --  FLAG
      end if;

      I := I * (I + 1);
   end Proc;

   function Factorial (I : Natural) return Positive is
   begin
      if I = 0 then
         return 1;
      else
         return I * Factorial (I - 1);    --  FLAG
      end if;
   exception
      when Constraint_Error =>
         return Natural'Last;             --  FLAG
   end Factorial;

begin
   null;
end Ret;
