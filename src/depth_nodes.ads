with Options;
with Iters.Iterators;

with Libadalang.Analysis;

with Ada.Containers;
with Ada.Containers.Vectors;

package Depth_Nodes is

   type Depth_Node is record
      Depth : Natural;
      Node  : Libadalang.Analysis.Ada_Node;
   end record;
   --  Depth-mapped AST node used in (and returned by) selectors

   package Depth_Node_Vectors is new Ada.Containers.Vectors
     (Positive, Depth_Node);
   --  Vectors of Depth_Node values

   subtype Depth_Node_Vector is Depth_Node_Vectors.Vector;
   --  Vector of Depth_Node values

   package Depth_Node_Iters is new Iters.Iterators (Depth_Node);
   --  Iterators over Depth_Node values

   subtype Depth_Node_Iter is
     Depth_Node_Iters.Iterator_Interface;
   --  Interface implemented by iterators over Depth_Node values

   subtype Depth_Node_Iter_Access is Depth_Node_Iters.Iterator_Access;
   --  Pointer to an iterator over Depth_Node values

   package Depth_Node_Options is new Options (Depth_Node);
   --  Optionnal Depth_Node values

   subtype Depth_Node_Option is Depth_Node_Options.Option;
   --  Optionnal Depth_Node value

   type Depth_Node_Array is array (Positive range <>) of Depth_Node;

   function Hash (Value : Depth_Node) return Ada.Containers.Hash_Type;
   --  Return the has of a Depth_Node value.
   --  The hash is computedt by xoring the hash of the ast node and it's
   --  depth value.

end Depth_Nodes;
