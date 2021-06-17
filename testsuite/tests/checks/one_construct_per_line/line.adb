package body Line is

   procedure Proc (I : in out Integer) is
      Tmp : Integer;

      procedure Proc1 (I : in out Integer; J : Integer);   --  NO FLAG
      B1, B2 : Boolean;                                    --  NO FLAG
      B3 : Boolean; B4 : Boolean;                          --  FLAG

      procedure Proc1 (I : in out Integer; J : Integer) is --  NO FLAG
      begin
         I := J; if I > 0 then                             --  FLAG
           I := 0; end if;                                 --  FLAG
      end Proc1;

   begin
      for J in 1 .. 10 loop                                --  NO FLAG
         null;
      end loop;

      Tmp := I; I := I + 1;                                --  FLAG
      I := I + Tmp;                                        --  NO FLAG
      I := I + 1; end Proc;                                --  FLAG
end Line;
