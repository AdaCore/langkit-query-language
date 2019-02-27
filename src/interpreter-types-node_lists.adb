with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Interpreter.Types.Node_Lists is

   -------------
   -- Display --
   -------------

   procedure Display (Value : Node_List) is
   begin
      for Node of Value.Nodes loop
         Put_Line (Node.Text_Image);
      end loop;
   end Display;

end Interpreter.Types.Node_Lists;
