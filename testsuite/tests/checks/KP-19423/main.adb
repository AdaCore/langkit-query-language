procedure Main is
   type Int_Array is array (Positive range <>) of Integer;
   A : Int_Array (1 .. 3) := (1, 2, 3);
   function F return Int_Array is (A);
   function G (X : Integer) return Int_Array is (A);
begin
   for I in F'Range loop      --  FLAG
      null;
   end loop;

   for I of F loop            --  NOFLAG
      null;
   end loop;

   for I in G (0)'Range loop  --  FLAG
      null;
   end loop;

   for I in A'Range loop      --  NOFLAG
      null;
   end loop;
end Main;
