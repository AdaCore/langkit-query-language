with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Interpreter.Types.NodeLists is

   -------------
   -- Display --
   -------------

   procedure Display (Value : NodeList) is
   begin
      for Node of Value.nodes loop
         Put_Line (Node.Text_Image);
      end loop;
   end Display;

end Interpreter.Types.NodeLists;
