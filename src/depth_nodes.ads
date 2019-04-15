with Iters.Iterators;

with Libadalang.Analysis;

with Ada.Containers; use Ada.Containers;

package Depth_Nodes is

   type Depth_Node is record
      Depth : Natural;
      Node  : Libadalang.Analysis.Ada_Node;
   end record;
   --  Depth-mapped AST node used in (and returned by) selectors

   package Depth_Node_Iters is new Iters.Iterators (Depth_Node);
   --  Iterators over Depth_Node values

   subtype Depth_Node_Iter is
     Depth_Node_Iters.Iterator_Interface;
   --  Interface implemented by iterators over Depth_Node values

   subtype Depth_Node_Iter_Access is Depth_Node_Iters.Iterator_Access;
   --  Pointer to an iterator over Depth_Node values

   function Hash (Value : Depth_Node) return Ada.Containers.Hash_Type;
   --  Return the has of a Depth_Node value.
   --  The hash is computedt by xoring the hash of the ast node and it's
   --  depth value.

end Depth_Nodes;
