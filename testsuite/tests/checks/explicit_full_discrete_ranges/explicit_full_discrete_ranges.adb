package body Explicit_Full_Discrete_Ranges is
   procedure P is
      subtype Idx is Integer range 1 .. 100;
      K, L : Integer := 0;
   begin
      for J in Idx'First .. Idx'Last loop   --  FLAG
         K := K + J;
      end loop;

      if K in Idx'First .. Idx'Last then    --  FLAG
         null;
      end if;

      for J in Idx loop                     --  NO FLAG
         L := L + J;
      end loop;
   end P;
end Explicit_Full_Discrete_Ranges;
