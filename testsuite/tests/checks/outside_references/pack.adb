package body Pack is

   procedure Proc (J : in out Integer) is
      K : Integer := J;                            --  NO FLAG
      Local_Rec : Rec;

      Var_Arr_Local : Arr;
      Var_Arr_Local_R : Integer renames Var_Arr_Local (5);

      procedure Inner_Proc (N : in out Integer) is
      begin
         N := I +                                  --  NO FLAG
              K +                                  --  FLAG
              J +                                  --  FLAG
              Global_Rec.C +                       --  NO FLAG
              Local_Rec.C;                         --  FLAG
      end Inner_Proc;

      procedure Inner2 (I : out Integer) is
         I1 : Integer renames Var_Arr_Local_R;      --  FLAG
      begin
         I := I1;                                   --  FLAG
      end Inner2;

   begin                                           --  NO FLAG
      I := J;                                      --  NO FLAG
      J := K + 1;                                  --  NO FLAG
   end Proc;

end Pack;
