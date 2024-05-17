procedure Main is
   task T is
      entry X;  --  FLAG
      entry Y;
      entry Z;
   end T;

   task body T is
   begin
      null;
   end T;

   type Subp_Access is access procedure;  --  FLAG
begin
   null;
end Main;
