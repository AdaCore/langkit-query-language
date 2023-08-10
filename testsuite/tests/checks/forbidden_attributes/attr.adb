procedure Attr is
   type Arr is array (1 .. 10) of Integer;
   Arr_Var : Arr;

   subtype Ind is Integer range Arr'First .. Arr'Last; --  FLAG (2)
begin
   for J in Arr'Range loop                             --  FLAG
      Arr_Var (J) := Integer'Succ (J);                 --  NOFLAG
end Attr;
