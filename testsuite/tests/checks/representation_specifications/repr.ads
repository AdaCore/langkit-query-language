package Repr is

   type State is (A,M,W,P);
   for State use (A => 1, M => 2, W => 3, P => 4); --  FLAG

   type Byte_Mask     is array (0..7)  of Boolean; --  FLAG
   for Byte_Mask'Component_Size use 1;

   type State_Mask    is array (State) of Boolean  --  FLAG
     with Component_Size => 1;

   Var : Integer;                                  --  FLAG
   pragma Volatile (Var);

   function F return Boolean with No_Return;       --  FLAG

   function F2 return Boolean;    --  FLAG missing pending U330-018
   pragma No_Return (F2);

end Repr;
