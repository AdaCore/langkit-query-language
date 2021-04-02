procedure Test is
   This_Is_A_Very_Very_Very_Long_Variable_Name : Integer := 42;  --  FLAG

   procedure This_Is_A_Very_Very_Very_Long_Subprogam_Name is null;  --  FLAG

   type This_Is_A_Very_Very_Very_Long_Type_Name is null record;  --  FLAG

   type Enum_Type is
     (This_Is_A_Very_Very_Very_Long_Enum_Literal_Name);  --  NO FLAG
begin
   null;
end Test;
