procedure Main is
   type Acc_Int is access Integer;
   subtype S_Acc_Int is Acc_Int;
   type D_Acc_Int is new Acc_Int;

   type Arr_Acc_Int is array (1 .. 5) of Acc_Int;
   type Arr_S_Acc_Int is array (1 .. 5) of S_Acc_Int;
   type Arr_D_Acc_Int is array (1 .. 5) of D_Acc_Int;
   subtype S_Arr_Acc_Int is Arr_Acc_Int;
   type D_Arr_Acc_Int is new Arr_Acc_Int;
   type Arr_Ano_Acc_Int is array (1 .. 5) of access Integer;
   type Arr_Int is array (1 .. 5) of Integer;

   type Arr_Acc is access Arr_Acc_Int;          --  FLAG
   type Arr_S_Acc is access Arr_S_Acc_Int;      --  FLAG
   type Arr_D_Acc is access Arr_D_Acc_Int;      --  FLAG
   type S_Arr_Acc is access S_Arr_Acc_Int;      --  FLAG
   type D_Arr_Acc is access D_Arr_Acc_Int;      --  FLAG
   type Ano_Arr_Acc is access Arr_Ano_Acc_Int;  --  FLAG
   type Arr is access Arr_Int;                  --  NOFLAG
begin
   null;
end Main;
