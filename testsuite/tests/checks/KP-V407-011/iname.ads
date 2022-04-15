package IName is
   pragma Pure;

   type Object is limited interface;
   function Name (This : in Object) return String is abstract;

end IName;
