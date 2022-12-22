package body Actuals is

   function Info2 return Integer is (1);

   procedure Subp1 (I : Integer := 0; Info : Integer) is null;

   procedure Subp2 is
   begin
      Subp1 (Info => Integer'(Info2));   --  FLAG
   end Subp2;

end;
