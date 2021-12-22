procedure Slice is
   type Table_Array_Type is array (1 .. 10) of Integer;
   Primary_Table   : Table_Array_Type;
   Secondary_Table : Table_Array_Type;

begin
   for I in Table_Array_Type'Range loop   --  FLAG
      Secondary_Table (I) := Primary_Table(I);
   end loop;
end Slice;
