with Ada.Unchecked_Conversion;

package body Main is

    function Int_Array_To_Byte_Array is new Ada.Unchecked_Conversion
      (Source => Int_Array,
       Target => Byte_Array);

    function Renamed_Conversion
      (S : Int_Array) return Byte_Array renames Int_Array_To_Byte_Array;

    function Byte_Array_To_Char_Array is new Ada.Unchecked_Conversion
      (Source => Byte_Array,
       Target => Char_Array);

    function Char_Array_To_Byte_Array is new Ada.Unchecked_Conversion
      (Char_Array, Byte_Array);

    function Int_Array_To_Sub_Byte_Array is new Ada.Unchecked_Conversion
      (Target => Sub_Byte_Array,
       Source => Int_Array);

    function Int_Array_To_New_Byte_Array is new Ada.Unchecked_Conversion
      (Source => Int_Array,
       Target => New_Byte_Array);

    function Int_Array_To_Private_Byte_Array is new Ada.Unchecked_Conversion
      (Source => Int_Array,
       Target => Private_Byte_Array);

    function Not_A_Conversion (A_Int_Array : Int_Array) return Byte_Array is ((others => 0));

    procedure Test (A_Byte_Array : Byte_Array) is null;
    procedure Test (A_Char_Array : Char_Array) is null;
    procedure Test (A_Sub_Byte_Array : Sub_Byte_Array) is null;
    procedure Test (A_New_Byte_Array : New_Byte_Array) is null;
    procedure Test (A_Private_Byte_Array : Private_Byte_Array) is null;

    My_Byte_Array : Byte_Array := (others => 0);
    My_Char_Array : Char_Array := (others => '0');
    My_Int_Array : Int_Array := (others => 0);
    My_Sub_Byte_Array : Sub_Byte_Array := (others => 0);
    My_New_Byte_Array : New_Byte_Array := (others => 0);

    procedure My_Tests is
    begin
        Test (Int_Array_To_Byte_Array (My_Int_Array));            -- FLAG
        Test (Renamed_Conversion (My_Int_Array));                 -- FLAG
        Test (Char_Array_To_Byte_Array (My_Char_Array));          -- FLAG
        Test (Int_Array_To_Sub_Byte_Array (My_Int_Array));        -- FLAG
        Test (Int_Array_To_New_Byte_Array (My_Int_Array));        -- FLAG
        Test (Int_Array_To_Private_Byte_Array (My_Int_Array));    -- FLAG
        Test (My_Byte_Array);                                     -- NOFLAG
        Test (Not_A_Conversion (My_Int_Array));                   -- NOFLAG
        My_Byte_Array := Int_Array_To_Byte_Array (My_Int_Array);  -- NOFLAG
        Test (Byte_Array_To_Char_Array (My_Byte_Array));          -- NOFLAG
        Test
          (Byte_Array_To_Char_Array                               -- FLAG
            (Int_Array_To_Byte_Array (My_Int_Array)));
    end My_Tests;

end Main;
