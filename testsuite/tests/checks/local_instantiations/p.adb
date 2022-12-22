package body P is

   package body Generic2 is
      procedure Proc is null;
   end Generic2;

   procedure Proc2 is
      package Inst1 is new Generic1;  --  FLAG
      package Inst2 is new Generic2;  --  NO FLAG
   begin
      null;
   end;

end P;
