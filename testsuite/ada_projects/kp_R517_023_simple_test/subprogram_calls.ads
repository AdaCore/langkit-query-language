with Type_Declarations;       use Type_Declarations;
with Subprogram_Declarations; use Subprogram_Declarations;

package Subprogram_Calls is

  Var_Int : Integer;

   ----------------------------------------------
   --  Calls with these actuals may be flagged --
   ----------------------------------------------

   Var_Rec_1 : Rec_1;
   --  component's type is U_Arr_1
   Var_Rec_2 : Rec_2;
   --  component's type is U_Arr_2

   -----------------------------------------------------
   --  Calls with these actuals should not be flagged --
   -----------------------------------------------------

   Var_Rec_3 : Rec_3;
   --  component's type is U_Arr_1
   Var_Rec_4 : Rec_4;
   --  component's type is U_Arr_3
   Var_Rec_5 : Rec_5;
   --  component's type is C_Arr_1
   Var_Rec_6 : Rec_6;
   --  component's type is C_Arr_2

   procedure Proc;

end Subprogram_Calls;
