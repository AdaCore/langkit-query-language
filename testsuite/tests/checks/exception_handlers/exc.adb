procedure Exc is   --  FLAG
   task T;
   task T2;

   task body T is  --  FLAG
   begin
      null;
   end T;

   task body T2 is  --  FLAG
   begin
      null;
   exception
      when others => null;
   end T2;

   procedure Proc is  --  NO FLAG
   begin
      null;
   exception
      when others => null;
   end Proc;

begin
   null;
end Exc;
