with Impl_A;
with Impl_B;

package Main_Pkg is

   type Object(<>) is tagged limited private;
   type Object_Access is access all Object'Class;

   function Instance return Object_Access;
   procedure Start (This : not null access Object);

private
   
   type Object is tagged limited record
      M_Impl_A : aliased Impl_A.Object;
      M_Impl_B : aliased Impl_B.Object;
   end record;
   
end Main_Pkg;
