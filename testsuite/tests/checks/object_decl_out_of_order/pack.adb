package body Pack is

   procedure Proc1 is separate;

   I : Integer;    --  FLAG

   procedure Proc is
      procedure Inner is
      begin
         null;
      end;

      X : T;       --  NOFLAG

   begin
      null;
   end;

end Pack;
