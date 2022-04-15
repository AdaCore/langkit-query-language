package P is
   type Index_Type is new Natural;
   type Element_Type is new Integer;

   type Vector is private with
     Default_Initial_Condition => Length (Vector) = 0;

   function Length (Container : in Vector) return Index_Type;
   function "&" (Left : in Vector; Right : in Element_Type) return Vector;

private

   type Array_Type is array (1 .. 100) of aliased Element_Type;

   type Vector is record
      Elements : Array_Type;
      Last     : Index_Type := 0;
   end record;

end P;
