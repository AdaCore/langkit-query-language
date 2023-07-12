package body Line is

   procedure Proc (I : in out Integer) is
      Tmp : Integer;

      procedure Proc1 (I : in out Integer; J : Integer);   --  NOFLAG
      B1, B2 : Boolean;                                    --  NOFLAG
      B3 : Boolean; B4 : Boolean;                          --  FLAG (2)

      procedure Proc1 (I : in out Integer; J : Integer) is --  NOFLAG
      begin
         I := J; if I > 0 then                             --  FLAG (2)
           I := 0; end if;                                 --  FLAG
      end Proc1;

   begin
      for J in 1 .. 10 loop                                --  NOFLAG
         null;
      end loop;

      Tmp := I; I := I + 1;                                --  FLAG (2)
      I := I + Tmp;                                        --  NOFLAG
      I := I + 1; end Proc;                                --  FLAG
end Line;
