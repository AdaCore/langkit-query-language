with Generics; use Generics;

package Instances is

   function My_Image is new Image;

   package My_Services is
     new Services (Ret_Type => String, Map_Value => My_image);
end;
