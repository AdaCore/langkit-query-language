with R; use R;

package Q is

  type Ext is new Root with null record;

  type Ext_Access is access all Ext'Class;

  type Rec is record
    Obj : Ext_Access;
  end record;

  function Get_Value (R : in out Rec) return Integer;

  function Get_Ref2 (Self : aliased in out Ext) return Reference;

end Q;
