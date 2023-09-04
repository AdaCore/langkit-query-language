procedure Tasks is
   task T1;
   task T2;
   task T3;

   task body T1 is  --  FLAG
   begin
      null;
   end T1;

   task body T2 is  --  FLAG
   begin
      null;
   exception
      when Constraint_Error => null;
   end T2;

   task body T2 is  --  NOFLAG
   begin
      null;
   exception
      when others => null;
   end T3;

begin
   null;
end Tasks;
