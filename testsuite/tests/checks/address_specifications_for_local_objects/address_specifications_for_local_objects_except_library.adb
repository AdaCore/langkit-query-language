with Address_Specifications_For_Local_Objects;

procedure Address_Specifications_For_Local_Objects_Except_Library (I : in out Integer) is
   Tmp : Integer with Address =>
     Address_Specifications_For_Local_Objects.Var'Address;   --  NOFLAG
begin
   I := Tmp;
end Address_Specifications_For_Local_Objects_Except_Library;
