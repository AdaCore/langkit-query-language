package body Pack is
   Z1 : Integer;          --  FLAG
   Z2 : Integer := 12;    -- NOFLAG

   procedure Proc (X : in out Integer) is
      Tmp : Integer;      -- NOFLAG

      package Inner is
         J1 : Integer;          -- NOFLAG
         J2 : Integer := 13;    -- NOFLAG
      end Inner;

   begin
      null;
   end Proc;

end Pack;
