package P is
   type Parent is tagged null record;
   type Ref is tagged private;

   procedure Set (Self : in out Ref'Class; Data : Parent'Class);

   type Reference_Type   --  FLAG
     (Element : access Parent'Class) is limited null record
   with Implicit_Dereference => Element;

   function Get (Self : Ref'Class) return Reference_Type;

private
   type Element_Access is access all Parent'Class;
   type Ref is tagged record
      Data : Element_Access;
   end record;
end P;
