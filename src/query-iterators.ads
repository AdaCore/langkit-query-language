with Options;
with Iters.Iterators;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

package Query.Iterators is

   type Iterator_Node is record
      Node  : LAL.Ada_Node;
      Depth : Natural;
   end record;
   --  Store an AST node along with it's depth

   package Iterator_Node_Options is new Options (Iterator_Node);
   use Iterator_Node_Options;
   --  Optional Iterator_Node values

   package Iterator_Node_Vectors is new Ada.Containers.Vectors
     (Positive, Iterator_Node);
   --  Vector of Iterator_Node values

   package Node_Iterators is new Iters.Iterators (Iterator_Node);
   --  Iterator over Iterator_Node values

   subtype Node_Iterator is Node_Iterators.Iterator_Interface;
   --  Interface implemented by iterator over Iterator_Node values

   subtype Node_Iterator_Access is Node_Iterators.Iterator_Access;
   --  Pointer to an iterator over Iterator_Node values

   type Childs_Iterator is new Node_Iterator with private;
   --  Iterator that yields the children of a given node

   overriding function Next (Iter : in out Childs_Iterator;
                             Element : out Iterator_Node)
                             return Boolean;
   --  Get the next children. If there is no children to yield
   --  anymore, return False. Otherwise, return True and set 'Result'.

   overriding procedure Release (Iter : in out Childs_Iterator);
   --  Release ressources that belong to 'Iter'

   function Make_Childs_Iterator (Node : LAL.Ada_Node) return Childs_Iterator;
   --  Return an iterator that yields all nodes under 'Node' (included) in
   --  a depth first search fashion.

private

   type Element_Vector_Access is access all Iterator_Node_Vectors.Vector;

   procedure Free_Element_Vector is new Ada.Unchecked_Deallocation
     (Iterator_Node_Vectors.Vector, Element_Vector_Access);

   type Childs_Iterator is new Node_Iterator with record
      Stack : Element_Vector_Access;
   end record;

   procedure Stack_Childs
     (Iter : in out Childs_Iterator; Element : Iterator_Node);
   --  Push the children of 'Element' onto 'Iter's stack

   function Pop
     (Stack  : Element_Vector_Access) return Option;
   --  Pop an element from 'Stack'

end Query.Iterators;
