procedure Main is
   type T is tagged null record;
   type U is new T with record I: Integer; end record;

   A : Long_Integer := T'Max_Size_In_Storage_Elements;  --  NOFLAG
   B : T'Class := U'(I => 1);  --  NOFLAG

   Size : Long_Integer := T'Class'Max_Size_In_Storage_Elements;  --  FLAG
begin
   null;
end;
