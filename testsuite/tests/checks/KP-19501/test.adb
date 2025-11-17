procedure Test is
  type Bit_T is range 0 .. 1;
  type Bits_T is array (Integer range <>) of Bit_T;

  N_Array_Actual_Size_C : constant := 100;
  subtype Message_Length_T is Integer range 0 .. N_Array_Actual_Size_C;

  function Get_Buffer return Bits_T is
     Buffer : constant Bits_T (0 .. N_Array_Actual_Size_C) := (others => 0);
  begin
     return Buffer;
  end Get_Buffer;

  function Check_Length_Mes (Length : in Message_Length_T) return Message_Length_T is
  begin
     return Length;
  end Check_Length_Mes;

  function Check_Length_Int (Length : in Integer) return Integer is
  begin
     return Length;
  end Check_Length_Int;

  procedure Reproduce_Bug is
    Message : constant Bits_T := Get_Buffer;
    Length_Mes : Message_Length_T;
    Length_Int : Integer;
  begin
     Length_Mes := Check_Length_Mes (Message'Length);  --  FLAG
     Length_Int := Check_Length_Int (Message'Length);  --  NOFLAG
  end Reproduce_Bug;
begin
    Reproduce_Bug;
end Test;
