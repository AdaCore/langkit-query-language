procedure P is
   C_String_20_Length : constant := 20;

   type T_String_20 is new String (1 .. C_String_20_Length);

   subtype T_Data_Id is T_String_20;
   C_Null_Data_Id : constant T_Data_Id := (others => ' ');

   Valid : boolean := true;

begin
   Valid := C_Null_Data_Id'Valid_Scalars;  --  FLAG
end;
