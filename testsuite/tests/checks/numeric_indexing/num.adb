package body Num is
    X : Integer := Var (2);      --  FLAG
    Y : Integer := Var (1 + 2);  --  NOFLAG
    Z : Integer := Var (2 - 1);  --  NOFLAG

   function "-" (I : Integer) return T_Index is
   begin
      case I is
         when 0 => return A;
         when  -1000 ..  -1 => return B;
         when others => return C;
      end case;
   end "-";

begin
  F := Var_T (-1);     --  NOFLAG
end Num;
