procedure Main is
   type Simple is array (Natural range <>) of Integer;
   type Matrix is array (Natural range <>, Natural range <>) of Integer;
   type Tensor is array (Natural range <>, Natural range <>, Natural range <>) of Integer;

   subtype Sub_Matrix is Matrix (0 .. 2, 0 .. 2);
   type Der_Matrix is new Sub_Matrix;
   subtype Sub_Tensor is Tensor (0 .. 2, 0 .. 2, 0 .. 2);
   type Der_Tensor is new Sub_Tensor;

   function Id (X : Integer) return Integer is (X);
   procedure Process_Sub_Matrix (M : Sub_Matrix) is
   begin
      null;
   end Process_Sub_Matrix;

   subtype U is Integer range 0 .. Id (2);

   I : Integer := 1;
   A_1 : Simple (1 .. 3) := (1, 2, 3);
   A_2 : Simple (1 .. Id (3)) := (1, 2, 3);

   S_1 : Simple (0 .. 2) := (0 .. 2 => 42);                                --  NOFLAG
   S_2 : Simple (0 .. 2) := (Id (3) .. Id (5) => 42);                      --  NOFLAG

   M_1 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (0 .. 2 => 42));            --  NOFLAG
   M_2 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (Id (3) .. Id (5) => 42));  --  FLAG
   M_3 : Matrix (0 .. 2, 0 .. 2) :=
     ((for I in Id (3) .. Id (5) => I), (42, 42, 42), (42, 42, 42));       --  FLAG
   M_4 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (U'Range => 42));           --  FLAG
   M_5 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (A_1'Range => 42));         --  NOFLAG
   M_6 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (A_2'Range => 42));         --  FLAG
   M_7 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (U => 42));                 --  FLAG
   M_8 : Matrix (0 .. 2, 0 .. 2) := (0 .. 2 => (I => 42));                 --  NOFLAG
   S_M_1 : Sub_Matrix := (0 .. 2 => (Id (3) .. Id (5) => 42));             --  FLAG
   S_M_2 : Sub_Matrix := (0 .. 2 => (3 .. Id (5) => 42));                  --  FLAG
   S_M_3 : Sub_Matrix := (Id (3) .. Id (5) => (0 .. 2 => 42));             --  NOFLAG
   D_M_1 : Der_Matrix := (0 .. 2 => (Id (3) .. Id (5) => 42));             --  FLAG

   T_1 : Tensor (0 .. 2, 0 .. 2, 0 .. 2) :=
     (0 .. 2 => (0 .. 2 => (0 .. 2 => 42)));                               --  NOFLAG
   T_2 : Tensor (0 .. 2, 0 .. 2, 0 .. 2) :=
     (0 .. 2 => (0 .. 2 => (Id (3) .. Id (5) => 42)));                     --  FLAG
   S_T_1 : Sub_Tensor := (0 .. 2 => (0 .. 2 => (Id (3) .. Id (5) => 42))); --  FLAG
   S_T_2 : Sub_Tensor := (0 .. 2 => (Id (3) .. Id (5) => (0 .. 2 => 42))); --  FLAG
   S_T_3 : Sub_Tensor := (Id (3) .. Id (5) => (0 .. 2 => (0 .. 2 => 42))); --  NOFLAG
   D_T_1 : Der_Tensor := (0 .. 2 => (0 .. 2 => (Id (3) .. Id (5) => 42))); --  FLAG
begin
   Process_Sub_Matrix (Sub_Matrix'(0 .. 2 => (Id (3) .. Id (5) => 42)));   --  FLAG
end Main;
