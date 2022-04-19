package P is

   type Lim_Rec is limited record
      A : Integer;
   end record;

   type Lim_Array is array (Positive range <>) of Lim_Rec;

   function Func_Lim_Array (Int : Integer) return Lim_Array;  --  FLAG

end P;
