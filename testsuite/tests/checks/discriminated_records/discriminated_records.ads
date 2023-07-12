package Discriminated_Records is
    type Idx is range 1 .. 100;
    type Arr is array (Idx range <>) of Integer;
    subtype Arr_10 is Arr (1 .. 10);

    type Rec_1 (D : Idx) is record        --  FLAG
       A : Arr (1 .. D);
    end record;

    type Rec_2 (D : Idx) is record        --  FLAG
       B : Boolean;
    end record;

    type Rec_3 is record
       B : Boolean;
    end record;

    type Rec_2_Deriv (D : Idx) is new Rec_2(D); -- NOFLAG
    type Rec_3_Deriv (D_Other : Idx) is new Rec_2(D => D_Other); -- FLAG

    type Pv_Rec (D : Idx) is private; -- NOFLAG

private
    type Pv_Rec (D : Idx) is null record; -- FLAG

end Discriminated_Records;
