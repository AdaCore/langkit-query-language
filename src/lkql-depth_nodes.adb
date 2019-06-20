with Ada.Containers; use Ada.Containers;

package body LKQL.Depth_Nodes is

   ----------
   -- Hash --
   ----------

   function Hash (Value : Depth_Node) return Ada.Containers.Hash_Type is
    (Hash_Rc (Value.Node));

end LKQL.Depth_Nodes;
