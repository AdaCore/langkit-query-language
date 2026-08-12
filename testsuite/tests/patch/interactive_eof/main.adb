procedure Main is
   S : constant String := "Hello";
   C : Character;
begin
   --  First violation
   for I in S'Range loop
      C := S (I);
   end loop;

   --  Second violation
   for J in S'Range loop
      S (J) := C;
   end loop;
end Main;
