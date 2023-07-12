package P is

  type Rec (D : Boolean := False) is record   --  FLAG
    case D is
      when False  => null;
      when others => I : Integer;
    end case;
  end record with Dynamic_Predicate => True;

  type Rec2 (D : Boolean := False) is record  -- NOFLAG
    case D is
      when False  => null;
      when others => I : Integer;
    end case;
  end record;

  type Rec3 (D : Boolean := False) is record   -- NOFLAG
    case D is
      when False => null;
      when True  => I : Integer;
    end case;
  end record with Dynamic_Predicate => True;

end P;
