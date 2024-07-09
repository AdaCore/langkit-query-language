with Ada.Unchecked_Conversion;

package Pos is
   type Arr is array (1 .. 10) of Integer;

   type Rec is record
      C_Int  : Integer;
      C_Bool : Boolean;
      C_Char : Character;
   end record;

   Var_Arr_1 : Arr := (1, 2, 3, others => 0);  --  FLAG
   Var_Arr_1 : Arr := (others => 0);  --  NOFLAG
   Var_Arr_1 : Arr := (1 => 1, 2 => 2, 3 => 3, others => 0);  --  NOFLAG

   Var_Rec_1 : Rec := (C_Int => 1, C_Bool => True, C_Char => 'a');  --  NOFLAG
   Var_Rec_2 : Rec := (2, C_Bool => False, C_Char => 'b');   --  FLAG
   Var_Rec_3 : Rec := (1, True, 'c');                        --  FLAG
end Pos;
