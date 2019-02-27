with Ada.Containers.Vectors;
with Libadalang.Analysis; use Libadalang.Analysis;

package Interpreter.Types.Node_Lists is

   package Nodes_Vector is new
     Ada.Containers.Vectors (Index_Type   => Natural,
                             Element_Type => LAL.Ada_Node);
   use Nodes_Vector;

   type Node_List is record
      Nodes : Nodes_Vector.Vector;
   end record;
   --  Store a vector of AST nodes, resulting from the execution of a query

   -----------------
   -- Text output --
   -----------------

   procedure Display (Value : Node_List);
   --  Print a NodeList value onto the console.

end Interpreter.Types.Node_Lists;
