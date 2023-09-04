package body Generics is

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
   begin
      return "";
   end;

   ----------------
   --  Services  --
   ----------------

   package body Services is

      -----------
      --  Map  --
      -----------

      --  This one depends on the actual Ret_Type, possibly unconstrained.

      function Map (X : Integer) return Ret_Type is
      begin
         return Map_Value (X);
      end;

      --  Now come a couple of straight cases private to the generic body,
      --  coming live as soon as the generic is instantiated.

      function Local_Blank return String;  --  FLAG

      -----------------
      -- Local Image --
      -----------------

      --  Returns unconstrained, acts as spec.

      function Local_Image (X : Integer) return String is  --  FLAG
      begin
         return Local_Blank;
      end;

      -----------------
      -- Local_Blank --
      -----------------

      --  Completion of a separate declaration. Only the latter is flagged.

      function Local_Blank return String is --  NOFLAG
      begin
         return " ";
      end;
   end;

end;
