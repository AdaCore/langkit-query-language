
package body Root.Child is

   procedure Primitive_1 (X : in out T_Child) is
   begin
      Primitive_1 (T_Root (X));      --  NO FLAG
      Primitive_2 (T_Root (X));      --  FLAG
      Primitive_2 (X);               --  FLAG
   end Primitive_1;

   procedure Primitive_2 (X : in out T_Child) is
   begin
      X.Comp  := X.Comp + 1;
   end Primitive_2;

end Root.Child;
