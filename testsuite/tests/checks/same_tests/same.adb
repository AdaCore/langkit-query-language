procedure Same (Str : String) is
   A : constant String := "toto";
   B : constant String := "titi";
begin
   if Str = A then                --  FLAG with line 9
      Put_Line("Hello, tata!");
   elsif Str = B then
      Put_Line("Hello, titi!");
   elsif Str = A then
      Put_Line("Hello, toto!");
   else
      Put_Line("Hello, world!");
   end if;
end Same;
