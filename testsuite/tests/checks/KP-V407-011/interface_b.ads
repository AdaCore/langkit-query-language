package Interface_B is

   type Object is limited interface;
   type Object_Access is access all Object'Class;
   
   procedure Do_B (This : not null access Object) is abstract;

end Interface_B;
