procedure Main is
   Excluded : constant Integer := 42;  --  NOFLAG
   VALID : constant Boolean := True;   --  NOFLAG
   Invalid : constant String := ":)"   --  FLAG

   type Valid_Type is new Integer;     --  NOFLAG
   type Invalid_type is new Integer;   --  FLAG
begin
   null;
end Main;
