with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Message         : String := "Hello World !";
   ConstantMessage : constant String := "Hello Constant world !";
begin
   Put_Line (Message);
end Main;
