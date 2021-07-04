procedure Anon (X : access Some_Type) is   --  NO FLAG
   type Square 
     (Location : access Coordinate)        --  NO FLAG
   is record
      null;
   end record;

   type Cell is record
      Some_Data : Integer;
      Next      : access Cell;             --  FLAG
   end record;

   Link : access Cell;                     --  FLAG

begin
   null;
end Anon;
