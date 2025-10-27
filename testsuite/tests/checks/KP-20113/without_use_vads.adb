with With_Use_Vads; use With_Use_Vads;

procedure Without_Use_Vads is
   type Constr_Arr is array (1 .. 2) of Integer;
   Constr_Arr_Size      : Integer := Constr_Arr'Size;            --  NOFLAG
   Constr_Arr_VADS_Size : Integer := Constr_Arr'VADS_Size;       --  FLAG
   Constr_Arr_Priv_Size : Integer := Constr_Arr_Priv'VADS_Size;  --  FLAG
begin
   null;
end Without_Use_Vads;
