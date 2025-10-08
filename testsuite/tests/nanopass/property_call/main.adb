with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Original : String (1 .. 13) := "Hello, World!";
   Reversed  : String (1 .. 13);
   function Reverse_String (Input : String) return String is
      Result : String (1 .. 13);
   begin
      for I in Input'Range loop
         Result (Input'First + Input'Last - I) := Input (I);
      end loop;
      return Result;
   end Reverse_String;
begin
   Reversed := Reverse_String (Original);
   Put_Line ("Original: " & Original);
   Put_Line ("Reversed: " & Reversed);
end Main;
