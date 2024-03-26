with Ada.Unchecked_Conversion;

package body Main is
    package body Types is
        function New_Private_Mutable return Private_Mutable is
          (True, Char_Comp => 'A');
    end Types;

    package body Tests is
        -- Test implicit overriding
        procedure Test_Implicit (I : Impl; Mut : out Mutable) is  -- NOFLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Implicit;

        type Implicit is new Implicit_Base with null record;
        procedure Test_Implicit (I : Implicit; Mut : out Mutable) is  -- FLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Implicit;

        procedure Not_Overriding (I : Implicit; Mut : out Mutable) is  -- NOFLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Not_Overriding;

        -- Test explicit overriding
        type Explicit is new Explicit_Base with null record;
        overriding procedure Test_Explicit (I : Explicit; Mut : out Mutable) is  -- FLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Explicit;

        -- Test explicit with early overriding
        type Explicit_Early is new Explicit_Base with null record;
        overriding procedure Test_Explicit (I : Explicit_Early; Mut : out Mutable);
        overriding procedure Test_Explicit (I : Explicit_Early; Mut : out Mutable) is  -- NOFLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Explicit;

        -- Test subtyping
        type Sub is new Sub_Base with null record;
        procedure Test_Subtype (I : Sub; Mut : out Sub_Mutable) is  -- FLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Subtype;

        -- Test derived type
        type Derived is new Derived_Base with null record;
        procedure Test_Derived (I : Derived; Mut : out Derived_Mutable) is  -- FLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Derived;

        -- Test private type
        type Priv is new Private_Base with null record;
        procedure Test_Private (I : Priv; Mut: out Private_Mutable) is  -- FLAG
        begin
            Mut := New_Private_Mutable;
        end Test_Private;

        -- Test read and write mutable parameter
        type Read_And_Write is new Read_And_Write_Base with null record;
        procedure Test_Read_And_Write (I : Read_And_Write; Mut : in out Mutable) is  -- FLAG
        begin
            Mut := (True, Char_Comp => 'A');
        end Test_Read_And_Write;

        -- Test assignment in exception handler
        type Exc_Hand is new Exc_Hand_Base with null record;
        procedure Test_Exc_Hand (I : Exc_Hand; Mut : out Mutable) is  -- FLAG
        begin
            null;
        exception
            when others =>
                Mut := (True, Char_Comp => 'A');
        end Test_Exc_Hand;

        -- Test non mutable discrimant
        type Not_Mut is new Not_Mut_Base with null record;
        procedure Test_Not_Mut (I : Not_Mut; NM : out Not_Mutable) is  -- NOFLAG
        begin
            NM := (True, Char_Comp => 'A');
        end Test_Not_Mut;

        -- Test constrained subtype
        type Constrained is new Constrained_Base with null record;
        procedure Test_Constrained (I : Constrained; Const : out Constrained_Rec) is  -- NOFLAG
        begin
            Const := (True, Char_Comp => 'A');
        end Test_Constrained;

        -- Test constrained derived
        type Derived_Constrained is new Derived_Constrained_Base with null record;
        procedure Test_Derived_Constrained (I : Derived_Constrained; Const : out Derived_Constrained_Rec) is  -- NOFLAG
        begin
            Const := (True, Char_Comp => 'A');
        end Test_Derived_Constrained;
    end Tests;
end Main;
