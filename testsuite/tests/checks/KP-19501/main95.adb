-- Same tests as for main.adb but using pragmas instead of
-- Static/Dynamic_Predicate aspects.
procedure Main95 is
   function Id (I : Integer) return Integer is (I);

   type Arr is array (Integer range <>) of Integer;
   subtype Stat_Arr is Arr (1 .. 10);
   subtype Dyn_Arr is Arr (1 .. Id (10));
   type Multidim_Dyn_Arr is array (1 .. 10, 1 .. Id (10)) of Integer;

   subtype Stat_Int is Integer range 1 .. 10;
   subtype Dyn_Int is Integer range Id (1) .. Id (10);

   subtype Stat_Pred_Int is Integer;
   pragma Predicate (Entity => Stat_Pred_Int, Check => Stat_Pred_Int in 1 .. 10);
   subtype Dyn_Pred_Int is Integer;
   pragma Predicate (Entity => Dyn_Pred_Int, Check =>  Dyn_Pred_Int < Id(50));

   S : String := "Hello";
   C_S : constant String := "world";

   procedure Process_Int (I : Integer) is
   begin
      null;
   end Process_Int;

   procedure Process_Stat_Int (I : Stat_Int) is
   begin
      null;
   end Process_Stat_Int;

   procedure Process_Dyn_Int (I : Dyn_Int) is
   begin
      null;
   end Process_Dyn_Int;

   procedure Process_Stat_Pred_Int (I : Stat_Pred_Int) is
   begin
      null;
   end Process_Stat_Pred_Int;

   procedure Process_Dyn_Pred_Int (I : Dyn_Pred_Int) is
   begin
      null;
   end Process_Dyn_Pred_Int;

   procedure Process_Multiple (I : Dyn_Pred_Int; J : Stat_Pred_Int) is
   begin
      null;
   end Process_Multiple;

   Stat_Assign : Stat_Int;
   Dyn_Assign  : Dyn_Int;

   Stat_Index : Stat_Arr;
   Dyn_Index  : Dyn_Arr;
   Mult_Index : Multidim_Dyn_Arr;

   Qual_Expr_1 : Stat_Int := Stat_Int'(C_S'Length);  --  NOFLAG
   Qual_Expr_2 : Dyn_Int := Dyn_Int'(S'Length);      --  NOFLAG
   Qual_Expr_3 : Dyn_Int := Dyn_Int'(C_S'Length);    --  FLAG
   Qual_Expr_4 : Dyn_Int := Dyn_Int'(C_S'Size);      --  NOFLAG
begin
   Process_Int (S'Length);              --  NOFLAG
   Process_Int (C_S'Length);            --  NOFLAG
   Process_Stat_Int (S'Length);         --  NOFLAG
   Process_Stat_Int (C_S'Length);       --  NOFLAG
   Process_Dyn_Int (S'Length);          --  NOFLAG
   Process_Dyn_Int (C_S'Length);        --  FLAG
   Process_Dyn_Int (C_S'Size);          --  NOFLAG
   Process_Stat_Pred_Int (S'Length);    --  NOFLAG
   Process_Stat_Pred_Int (C_S'Length);  --  NOFLAG
   Process_Dyn_Pred_Int (S'Length);     --  NOFLAG
   Process_Dyn_Pred_Int (C_S'Length);   --  FLAG
   Process_Dyn_Pred_Int (C_S'Size);     --  NOFLAG

   Process_Multiple (S'Length, S'Size);      --  NOFLAG
   Process_Multiple (C_S'Length, C_S'Size);  --  FLAG

   Stat_Assign := S'Length;      --  NOFLAG
   Stat_Assign := C_S'Length;    --  NOFLAG
   Dyn_Assign  := S'Length;      --  NOFLAG
   Dyn_Assign  := C_S'Length;    --  FLAG
   Dyn_Assign  := C_S'Size;      --  NOFLAG

   Stat_Index (S'Length) := 10;        --  NOFLAG
   Stat_Index (C_S'Length) := 10;      --  NOFLAG
   Dyn_Index (S'Length) := 10;         --  NOFLAG
   Dyn_Index (C_S'Length) := 10;       --  FLAG
   Dyn_Index (C_S'Size) := 10;         --  NOFLAG
   Dyn_Index (1) := 10;                --  NOFLAG
   Mult_Index (1, S'Length) := 10;     --  NOFLAG
   Mult_Index (1, C_S'Length) := 10;   --  FLAG
   Mult_Index (1, C_S'Size) := 10;     --  NOFLAG
   Mult_Index (C_S'Length, 1) := 10;   --  FLAG
   Mult_Index (1, 1) := 10;            --  NOFLAG
end Main95;
