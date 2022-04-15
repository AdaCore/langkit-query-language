package body P is
   function Length (Container : in Vector) return Index_Type
   is (Container.Last);

   function "&" (Left : in Vector; Right : in Element_Type) return Vector is
   begin
      return Result : Vector := Left do   --  FLAG
         null;
      end return;
   end "&";

end P;
