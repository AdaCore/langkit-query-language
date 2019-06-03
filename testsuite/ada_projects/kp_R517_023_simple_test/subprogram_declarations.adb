package body Subprogram_Declarations is

   procedure F_Proc1 (X : in out U_Arr_1) is
   begin
      null;
   end;

   procedure F_Proc2
     (X :     U_Arr_1;
      Y : out U_Arr_2) is
   begin
      null;
   end;

   procedure F_Proc3
     (I : in out Integer;
      X :    out U_Arr_2) is
   begin
      null;
   end;


   procedure NF_Proc1 (X : in U_Arr_1) is
   begin
      null;
   end;

   procedure NF_Proc2
     (X : U_Arr_1;
      Y :  U_Arr_2) is
   begin
      null;
   end;

   procedure NF_Proc3
     (I : in out Integer;
      X :    out U_Arr_3) is
   begin
      null;
   end;

   procedure NF_Proc4
     (I : in out Integer;
      X :    out C_Arr_1) is
   begin
      null;
   end;

   procedure NF_Proc5
     (I :        Integer;
      X : in out C_Arr_2) is
   begin
      null;
   end;

   procedure NF_Proc6 (X : in out S_U_Arr_1) is
   begin
      null;
   end;

   procedure NF_Proc7
     (X :     U_Arr_1;
      Y : out S_U_Arr_2) is
   begin
      null;
   end;

   procedure NF_Proc8
     (I : in out Integer;
      X :    out S_U_Arr_2) is
   begin
      null;
   end;

end Subprogram_Declarations;
