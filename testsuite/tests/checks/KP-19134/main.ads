package Main is

    type Byte is range -127 .. 127;

    type Char_Array is array (1 .. 5) of Character;
    type Byte_Array is array (1 .. 5) of Byte;
    type Int_Array is array (1 .. 5) of Integer;
    subtype Sub_Byte_Array is Byte_Array;
    type New_Byte_Array is new Sub_Byte_Array;
    type Private_Byte_Array is private;

    procedure My_Tests;

private
    type Private_Byte_Array is array (1 .. 5) of Byte;
end Main;