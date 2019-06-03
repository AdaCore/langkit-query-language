with Type_Declarations; use Type_Declarations;

package Subprogram_Declarations is

   ------------------------------------------------
   --  Calls to these subprograms may be flagged --
   ------------------------------------------------

   procedure F_Proc1 (X : in out U_Arr_1);

   procedure F_Proc2
     (X :     U_Arr_1;
      Y : out U_Arr_2);

   procedure F_Proc3
     (I : in out Integer;
      X :    out U_Arr_2);

   -------------------------------------------------------
   --  Calls to these subprograms should not be flagged --
   -------------------------------------------------------

   procedure NF_Proc1 (X : in U_Arr_1);
   --  Parameter is not OUT/IT OUT

   procedure NF_Proc2
     (X : U_Arr_1;
      Y :  U_Arr_2);
   --  Parameters are not OUT/IT OUT

   procedure NF_Proc3
     (I : in out Integer;
      X :    out U_Arr_3);
   --  parameter of non-packed type

   procedure NF_Proc4
     (I : in out Integer;
      X :    out C_Arr_1);
   --  parameter of constrained type

   procedure NF_Proc5
     (I :        Integer;
      X : in out C_Arr_2);
   --  parameter of non-packed constrained type

   procedure NF_Proc6 (X : in out S_U_Arr_1);
   --  parameter is of constrained subtype

   procedure NF_Proc7
     (X :     U_Arr_1;
      Y : out S_U_Arr_2);
   --  parameter is of constrained subtype

   procedure NF_Proc8
     (I : in out Integer;
      X :    out S_U_Arr_2);
   --  parameter is of constrained subtype

end Subprogram_Declarations;
