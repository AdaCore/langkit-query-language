procedure Slice is
   type Table_Array_Type is array (1 .. 10) of Integer;
   Primary_Table   : Table_Array_Type;
   Secondary_Table : Table_Array_Type;
   S : String (1 .. 10);

   function Func (I : Integer) return Integer is (I);

begin
   for I in Table_Array_Type'Range loop   --  FLAG
      Secondary_Table (I) := Primary_Table (I);
   end loop;

   for I in Table_Array_Type'Range loop   --  FLAG
      Secondary_Table (I) := 10;
   end loop;

   for I in S'Range loop   --  FLAG
      S (I) := ' ';
   end loop;

   for I in Table_Array_Type'Range loop   --  NO FLAG
      Secondary_Table (I) := Primary_Table (Func (I));
   end loop;

end Slice;
