package P is

   type T_Index is (A, B, C);

   type T is array (T_Index) of Float;

   Var_T : T := (others => 0.0);

   F : Float;

   type Arr is array (-10 .. 10) of Integer;

   Var : Arr := (10 => 1, others => 10);

   C1        : constant Integer := Var (1);  --  FLAG
   C0        : constant Integer := Var (0);  --  FLAG
   C_Minus_1 : constant Integer := Var (-1); --  FLAG

   function "-" (I : Integer) return T_Index;

end P;
