package Main is

    package Types is
        type Not_Mutable (Cond : Boolean) is record
            case Cond is
                when True =>
                    Char_Comp : Character;
                when False =>
                    Int_Comp : Integer;
            end case;
        end record;

        type Mutable (Cond : Boolean := True) is record
            case Cond is
                when True =>
                    Char_Comp : Character;
                when False =>
                    Int_Comp : Integer;
            end case;
        end record;

        subtype Sub_Mutable is Mutable;

        type Derived_Mutable is new Sub_Mutable;

        type Private_Mutable is private;

        subtype Constrained_Rec is Mutable (True);

        type Derived_Constrained_Rec is new Mutable (True);

        function New_Private_Mutable return Private_Mutable;
    private
        type Private_Mutable is new Derived_Mutable;
    end Types;

    package Tests is
        use Types;

        -- Implicit overriding
        type Implicit_Base is interface;
        procedure Test_Implicit (I : Implicit_Base; Mut : out Mutable) is abstract;

        type Impl is new Implicit_Base with null record;
        procedure Test_Implicit (I : Impl; Mut : out Mutable);

        -- Explicit overriding
        type Explicit_Base is interface;
        procedure Test_Explicit (I : Explicit_Base; Mut : out Mutable) is abstract;

        -- Subtype
        type Sub_Base is interface;
        procedure Test_Subtype (I : Sub_Base; Mut : out Sub_Mutable) is abstract;

        -- Derived
        type Derived_Base is interface;
        procedure Test_Derived (I : Derived_Base; Mut : out Derived_Mutable) is abstract;

        -- Private
        type Private_Base is interface;
        procedure Test_Private (I : Private_Base; Mut : out Private_Mutable) is abstract;

        -- Read and write
        type Read_And_Write_Base is interface;
        procedure Test_Read_And_Write (I : Read_And_Write_Base; Mut : in out Mutable) is abstract;

        -- Exception handler
        type Exc_Hand_Base is interface;
        procedure Test_Exc_Hand (I : Exc_Hand_Base; Mut : out Mutable) is abstract;

        -- Not mutable
        type Not_Mut_Base is interface;
        procedure Test_Not_Mut (I : Not_Mut_Base; NM : out Not_Mutable) is abstract;

        -- Constrained
        type Constrained_Base is interface;
        procedure Test_Constrained (I : Constrained_Base; Const : out Constrained_Rec) is abstract;

        -- Constrained derived
        type Derived_Constrained_Base is interface;
        procedure Test_Derived_Constrained (I : Derived_Constrained_Base; Const : out Derived_Constrained_Rec) is abstract;
    end Tests;

end Main;
