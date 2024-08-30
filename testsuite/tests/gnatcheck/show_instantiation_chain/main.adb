procedure Main is
   generic
   procedure Gen;

   procedure Gen is  --  FLAG
   begin
      Gen;
   end Gen;

   procedure Inst is new Gen;
begin
   null;
end Main;
