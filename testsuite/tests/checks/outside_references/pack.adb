package body Pack is

   procedure Proc (J : in out Integer) is
      K : Integer := J;                            -- NOFLAG
      Local_Rec : Rec;

      Var_Arr_Local : Arr;
      Var_Arr_Local_R : Integer renames Var_Arr_Local (5);

      procedure Inner_Proc (N : in out Integer) is
      begin
         N := I +                                  -- NOFLAG
              K +                                  --  FLAG
              J +                                  --  FLAG
              Global_Rec.C +                       -- NOFLAG
              Local_Rec.C;                         --  FLAG
      end Inner_Proc;

      procedure Inner2 (I : out Integer) is
         I1 : Integer renames Var_Arr_Local_R;      --  FLAG
      begin
         I := I1;                                   --  FLAG
      end Inner2;

   begin                                           -- NOFLAG
      I := J;                                      -- NOFLAG
      J := K + 1;                                  -- NOFLAG
   end Proc;

end Pack;
