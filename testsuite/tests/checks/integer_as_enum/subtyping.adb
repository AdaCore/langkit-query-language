procedure Subtyping is
    type Enum1 is range 1 .. 3;  -- NOFLAG
    subtype Enum1_S is Enum1;

    generic
       type Int_F is range <>;
    procedure Proc_G (X : in out Int_F);

    procedure Proc_G (X : in out Int_F) is
    begin
       X := X + 1;
    end Proc_G;

    procedure Proc_I is new Proc_G (Enum1_S);

    type Enum2 is range 1 .. 3;  -- NOFLAG
    subtype Enum2_S is Enum2;

    type Int is range 1 .. 10;   -- NOFLAG

    E : Enum2 := 1;
    I : Int   := 1;

    type Enum3 is range 1 .. 3;  -- NOFLAG
    subtype Enum3_S is Enum3;

    type Enum3_D is new Enum3_S; -- NOFLAG
    X : Enum3_D := 1;

begin
    E := Enum2_S (I);

    I := I + 1;
    X := X + 1;
end Subtyping;
