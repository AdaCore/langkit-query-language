package body Names is  --  FLAG
   procedure Proc is   --  FLAG
   begin
      null;
   end;

   procedure Proc2 is   --  NOFLAG
   begin
      null;
   end Proc2;

   task body Task1 is   --  FLAG
   begin
      null;
   end;

   task body Task2 is   --  NOFLAG
   begin null; end;

   protected body Prot1 is   --  FLAG
      procedure P is         --  FLAG
      begin
         null;
      end;
   end;

   protected body Prot2 is  --  NOFLAG
      --
      --
   end Prot2;

end;
