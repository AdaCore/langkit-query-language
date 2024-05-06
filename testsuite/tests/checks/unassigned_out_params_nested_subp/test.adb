procedure Test is
   procedure P
     (A : out Integer;  --  NOFLAG
      B : out Integer   --  NOFLAG
      )
   is
      procedure Init_A is
      begin
         A := 2;
      end Init_A;
   begin
      Init_A;

      declare
         procedure Init_B is
         begin
            B := 2;
         end Init_B;
      begin
         Init_B;
      end;
   end P;
begin
   null;
end Test;
