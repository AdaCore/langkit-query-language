procedure P is
   type Test_Data_Word_Type is mod 2 ** 16;
   type Test_Data_Type is array (1..32) of Test_Data_Word_Type;
   type Array_SSO is array (1..32) of Test_Data_Word_Type
     with Size => 512,
     Scalar_Storage_Order => System.High_Order_First;
   Result : constant Test_Data_Type := (others => 16);
   Marshalled_Result : Array_SSO;
   Unmarshalled_Result : Test_Data_Type;
begin
   Marshalled_Result := Array_SSO (Result);                    --  FLAG
   Unmarshalled_Result := Test_Data_Type (Marshalled_Result);  --  FLAG
end;
