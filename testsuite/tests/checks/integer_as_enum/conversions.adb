procedure Conversions is
    type Enum1 is range 1 .. 3;  --  NOFLAG
    type Enum2 is range 1 .. 3;  --  NOFLAG
    type Int is range 1 .. 10;   --  NOFLAG

    E1 : Enum1 := 1;
    E2 : Enum2 := 1;
    I  : Int   := 1;


begin
    E1 := Enum1 (I);
    I  := Int (E2);

    I := I + 1;
end Conversions;
