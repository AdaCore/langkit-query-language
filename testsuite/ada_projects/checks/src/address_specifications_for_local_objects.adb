package body Address_Specifications_For_Local_Objects is
   procedure Proc (I : in out Integer) is
      Tmp : Integer with Address =>
        Address_Specifications_For_Local_Objects.Var'Address;   --  FLAG
   begin
      I := Tmp;
   end Proc;
end Address_Specifications_For_Local_Objects;
