with Generics; use Generics;

package Instances is

   function My_Image is new Image;  -- FLAG

   package My_Services is  -- FLAG (3 times)
     new Services (Ret_Type => String, Map_Value => My_image);
end;
