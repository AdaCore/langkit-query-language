with Interface_A;
with Interface_B;

package Impl_A is

   type Object is limited new
     Interface_A.Object and 
     Interface_B.Object with private;

   type Object_Access is access all Object'Class;
   
   function Instance return Object_Access;
   
   overriding procedure Do_A (This : not null access Object);
   overriding procedure Do_B (This : not null access Object);
   overriding function Name (This : in Object) return String;
   
private
   
   type Object is limited new
     Interface_A.Object and
     Interface_B.Object with
   record
      Field : Integer := 1;
   end record;
         
end Impl_A;
