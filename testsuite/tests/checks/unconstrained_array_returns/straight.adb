package body Straight is

   -------------
   --  Blank  --
   -------------

   --  This function body isn't a completion, hence acts as a declaration.

   function Blank return String is  -- FLAG
   begin
      return " ";
   end Blank;

   -------------
   --  Image  --
   -------------

   --  This function body is the completion of a declaration in the spec of
   --  this package.  Only the declaration is expected to be flagged, when the
   --  spec is processed.

   function Image (X : Integer) return String is -- NOFLAG
   begin
      return Blank;
   end Image;

   ------------
   --  Nest  --
   ------------

   --  This subprogram is wrapper for checks over nested functions returning
   --  unconstrained. Replicate locally the library level checks performed by
   --  this unit.

   procedure Nest is

      function My_Image (X : Integer) return String;  -- FLAG

      function My_Blank return String is -- FLAG
      begin
         return " ";
      end My_Blank;

      function My_Image (X : Integer) return String is -- NOFLAG
      begin
         return My_Blank;
      end My_Image;
   begin
      null;
   end;

end;
