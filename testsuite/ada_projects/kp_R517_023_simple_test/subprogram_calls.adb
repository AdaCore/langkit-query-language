with Type_Declarations;       use Type_Declarations;
with Subprogram_Declarations; use Subprogram_Declarations;

package body Subprogram_Calls is

   procedure Proc is
   begin
      F_Proc1 (Var_Rec_1.C);                --  FLAG
      F_Proc1 (Var_Rec_3.C);                --  NO FLAG

      F_Proc2 (Var_Rec_3.C, Var_Rec_2.C);   --  FLAG
      F_Proc2 (Var_Rec_1.C, Var_Rec_2.C);   --  FLAG

      F_Proc3 (Var_Int, Var_Rec_2.C);       --  FLAG

      NF_Proc1 (Var_Rec_1.C);               --  NO FLAG
      NF_Proc1 (Var_Rec_3.C);               --  NO FLAG

      NF_Proc2 (Var_Rec_1.C, Var_Rec_2.C);  --  NO FLAG

      NF_Proc3 (Var_Int, Var_Rec_4.C);      --  NO FLAG

      NF_Proc4 (Var_Int, Var_Rec_5.C);      --  NO FLAG
      NF_Proc5 (Var_Int, Var_Rec_6.C);      --  NO FLAG


      NF_Proc6 (Var_Rec_1.C);               --  NO FLAG
      NF_Proc6 (Var_Rec_3.C);               --  NO FLAG

      NF_Proc7 (Var_Rec_3.C, Var_Rec_2.C);  --  NO FLAG
      NF_Proc7 (Var_Rec_1.C, Var_Rec_2.C);  --  NO FLAG

      NF_Proc8 (Var_Int, Var_Rec_2.C);      --  NO FLAG

   end Proc;

end Subprogram_Calls;
