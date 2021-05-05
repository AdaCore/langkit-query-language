package Root is
   type T_Root is tagged private;

   procedure Primitive_1 (X : in out T_Root);
   procedure Primitive_2 (X : in out T_Root);
private
   type T_Root is tagged record
      Comp : Integer;
   end record;
end Root;
