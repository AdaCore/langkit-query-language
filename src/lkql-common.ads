with Iters.Iterators;

with Libadalang.Iterators; use Libadalang.Iterators;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package LKQL.Common is

   package Node_Iterators is new Iters.Iterators (LAL.Ada_Node);
   --  Ada node iterators

   subtype Node_Iterator is Node_Iterators.Iterator_Interface;
   --  Ada node iterator

   subtype Node_Iterator_Access is Node_Iterators.Iterator_Access;
   --  Access to an Ada node iterator

   subtype Node_Iterator_Predicate is Node_Iterators.Predicates.Func;
   --  Predicates on 'Iterator_Node' values

   subtype Iterator_Predicate_Access is Node_Iterators.Predicate_Access;
   --  Pointer to a predicate on 'Iterator_Node' values

   package Node_Vectors is new Ada.Containers.Vectors
     (Element_Type => LAL.Ada_Node,
      Index_Type   => Positive,
      "="          => LAL."=");
   --  Vectors of Ada_Node values

   subtype Node_Vector is Node_Vectors.Vector;
   --  Vector of Ada_Node values

   ---------------------
   -- Childs_Iterator --
   ---------------------

   type Traverse_Iterator_Access is access all Traverse_Iterator'Class;

   procedure Free_Traverse_Iterator is new Ada.Unchecked_Deallocation
     (Traverse_Iterator'Class, Traverse_Iterator_Access);

   type Childs_Iterator is new Node_Iterator with record
      Inner : Traverse_Iterator_Access;
      Root  : LAL.Ada_Node;
   end record;
   --  Iterator that yields every node under 'Root'

   overriding function Next (Iter : in out Childs_Iterator;
                             Result : out LAL.Ada_Node) return Boolean;

   overriding function Clone (Iter : Childs_Iterator) return Childs_Iterator;

   overriding procedure Release (Iter : in out Childs_Iterator);

   function Make_Childs_Iterator (Root : LAL.Ada_Node) return Childs_Iterator;
   --  Return an iterator that yields every node under 'Root' in depth-first
   --  order.

end LKQL.Common;
