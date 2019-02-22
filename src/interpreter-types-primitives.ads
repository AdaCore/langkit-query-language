with Interpreter.Types.Atoms;     use Interpreter.Types.Atoms;
with Interpreter.Types.NodeLists; use Interpreter.Types.NodeLists;

package Interpreter.Types.Primitives is

   type Primitive_Kind is (Kind_Atom, Kind_Node, Kind_NodeList);

   type Node_Access is access constant LEL.LKQL_Node;

   type Primitive (Kind : Primitive_Kind := Kind_Atom) is record
      case Kind is
         when Kind_Atom =>
            Atom_Val : Atom;
         when Kind_NodeList =>
            NodeList_Val : NodeList;
         when Kind_Node =>
            Node_Val : LEL.LKQL_Node;
      end case;
   end record;

   procedure Display (Value : Primitive);

   function To_Primitive (A : Atom) return Primitive;
   function To_Primitive (N : LEL.LKQL_Node) return Primitive;

end Interpreter.Types.Primitives;
