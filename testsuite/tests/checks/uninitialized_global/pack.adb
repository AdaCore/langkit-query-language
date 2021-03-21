package body Pack is
   Z1 : Integer;          --  FLAG
   Z2 : Integer := 12;    --  NO FLAG

   procedure Proc (X : in out Integer) is
      Tmp : Integer;      --  NO FLAG

      package Inner is
         J1 : Integer;          --  NO FLAG
         J2 : Integer := 13;    --  NO FLAG
      end Inner;

   begin
      null;
   end Proc;

end Pack;
