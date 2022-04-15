with Interface_A;
with Interface_B;

package Impl_B is

   type Object is tagged limited private;
   type Object_Access is access all Object'Class;
   
   function Instance return Object_Access;
   
   procedure Initialize
     (This : not null access Object;
      A    : not null Interface_A.Object_Access;
      B    : not null Interface_B.Object_Access);
   
   procedure Run(This : not null access Object);

private
      
   type Object is tagged limited record
      M_A   : Interface_A.Object_Access;
      M_B   : Interface_B.Object_Access;
   end record;
   
end Impl_B;
