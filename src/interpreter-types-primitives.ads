with Interpreter.Types.Atoms;     use Interpreter.Types.Atoms;
with Interpreter.Types.NodeLists; use Interpreter.Types.NodeLists;

with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Types.Primitives is

   type Primitive_Kind is (Kind_Atom, Kind_Node, Kind_NodeList);
   --  Denotes the kind of a primitive value.

   type Primitive (Kind : Primitive_Kind := Kind_Atom) is record
      case Kind is
         when Kind_Atom =>
            Atom_Val : Atom;
         when Kind_NodeList =>
            NodeList_Val : NodeList;
         when Kind_Node =>
            Node_Val : LAL.Ada_Node;
      end case;
   end record;
   --  Store a primitive value, which can be an atomic type
   --  (Bool, Int, ...), an AST node, or a list of AST nodes.

   -----------------------------------
   --  Creation of Primitive values --
   -----------------------------------

   function To_Primitive (A : Atom) return Primitive;
   --  Create a Primitive value from the Atom value.

   function To_Primitive (N : LAL.Ada_Node) return Primitive;
   --  Create a Primitive value from the LKQL_Node value.

   function To_Primitive (B : Boolean) return Primitive;
   --  Create a Bool primitive.

   function To_Primitive (I : Integer) return Primitive;
   --  Create an Int primitive.

   function To_Primitive (Val : Unbounded_Text_Type) return Primitive;
   --  Create a String primitive;

   ------------------------------
   -- Text conversion & output --
   ------------------------------

   function Kind_Name (Value : Primitive) return String;

   procedure Display (Value : Primitive);
   --  Print a Primitive value onto the console

end Interpreter.Types.Primitives;
