package body Line is

   task type Tsk is
      entry Start (I : Integer; B : Boolean);               --  NOFLAG
   end Tsk;

   task type Other_Tsk is
      entry Start (I : Integer; B : Boolean);               --  NOFLAG
   end Other_Tsk;

   task body Tsk is
   begin
      accept Start (I : Integer; B : Boolean) do            --  NOFLAG
         null;
      end Start;
   end Tsk;

   task body Other_Tsk is
   begin
      accept Start (I : Integer; B : Boolean) do null;      --  FLAG
      end Start;
   end Other_Tsk;

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

      My_Loop:                                             --  NOFLAG
      for I in 1 .. 2 loop
         null;
      end loop My_Loop;

      My_Loop_2: for I in 1 .. 2 loop                      --  FLAG
         null;
      end loop My_Loop_2;

      Tmp := I; I := I + 1;                                --  FLAG (2)
      I := I + Tmp;                                        --  NOFLAG
      I := I + 1; end Proc;                                --  FLAG
end Line;
