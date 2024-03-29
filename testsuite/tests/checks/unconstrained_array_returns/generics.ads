package Generics is

   --  Generic declarations themselves are not flagged.

   generic
   function Image (X : Integer) return String;   --  FLAG 

   generic
      type Ret_Type (<>) is private;
      with function Map_Value (X : Integer) return Ret_Type;   --  NOFLAG
   package Services is
     function Map (X : Integer) return Ret_Type;    --  FLAG
   end;

end Generics;
