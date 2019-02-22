with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Interpreter.Types.Primitives is

   -------------
   -- Display --
   -------------

   procedure Display (Value : Primitive) is
   begin
      case Value.Kind is
         when Kind_Atom =>
            Display (Value.Atom_Val);
         when Kind_Node =>
            Put_Line (Value.Node_Val.Text);
         when Kind_NodeList =>
            Display (Value.NodeList_Val);
      end case;
   end Display;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (A : Atom) return Primitive is
   begin
      return (Kind => Kind_Atom, Atom_Val => A);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (N : LEL.LKQL_Node) return Primitive is
   begin
      return (Kind => Kind_Node, Node_Val => N);
   end To_Primitive;

end Interpreter.Types.Primitives;
