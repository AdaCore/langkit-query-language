package R is

  type Reference (Element : access Integer) is limited null record
    with Implicit_Dereference => Element;

  type Root is tagged record
    Value : aliased Integer;
  end record;

  function Get_Ref (Self : aliased in out Root) return Reference;  --  FLAG
  function Get_Ref2 (Self : aliased in out Root) return Reference;  --  NOFLAG: overridden
  function Prim2 (Self : aliased in out Root) return Integer;  --  NOFLAG: Inteer is not limited
  procedure Prim3 (Self : aliased in out Root);  --  NOFLAG: not a function

end R;
