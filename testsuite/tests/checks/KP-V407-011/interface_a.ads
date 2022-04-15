with IName;

package Interface_A is

   type Object is limited interface and IName.Object;
   type Object_Access is access all Object'Class;
   
   procedure Do_A (This : not null access Object) is abstract;
   
end Interface_A;
