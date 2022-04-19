procedure P is
  subtype Id_T is String (1 .. 2);

  type Enum is (None, Nam);

  type Rec(Discr : Enum := None) is record
    case Discr is
      when None => null;
      when Nam  => Id : Id_T;
    end case;
  end record;

  type Coord_T is record
    S_Before : String (1 .. 4);
    Field    : Rec;
  end record;
  for Coord_T use record
    S_Before at  0 range 0 .. 31;
    Field    at  4 range 0 .. 47;
  end record;

  C : Coord_T;

  function Get_Field (Discr : in Enum) return Rec with Import;

begin
  C.S_Before := (others => 'B');
  C.Field    := Rec'(Get_Field (None));   --  FLAG
end;
