with Ada.Containers.Vectors;
with Liblkqllang.Analysis; use Liblkqllang.Analysis;

package Interpreter.Types.NodeLists is

   package Nodes_Vector is new
     Ada.Containers.Vectors (Index_Type   => Natural,
                             Element_Type => LEL.LKQL_Node);
   use Nodes_Vector;

   type NodeList is record
      nodes : Nodes_Vector.Vector;
   end record;

   procedure Display (Value : NodeList);

end Interpreter.Types.NodeLists;
