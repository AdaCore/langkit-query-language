
package Root.Child is
   type T_Child is new T_Root with private;

   procedure Primitive_1 (X : in out T_Child);
   procedure Primitive_2 (X : in out T_Child);
private
   type T_Child is new T_Root with record
      B : Boolean;
   end record;
end Root.Child;
