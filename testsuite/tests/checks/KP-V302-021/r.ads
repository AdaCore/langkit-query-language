package R is

  type Reference (Element : access Integer) is limited null record
    with Implicit_Dereference => Element;

  type Root is tagged record
    Value : aliased Integer;
  end record;

  function Get_Ref (Self : aliased in out Root) return Reference;  --  FLAG
  function Get_Ref2 (Self : aliased in out Root) return Reference;  --  NO FLAG: overridden
  function Prim2 (Self : aliased in out Root) return Integer;  --  NO FLAG: Inteer is not limited
  procedure Prim3 (Self : aliased in out Root);  --  NO FLAG: not a function

end R;
