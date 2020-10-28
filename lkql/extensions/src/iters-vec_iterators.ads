with Iters.Iterators;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

generic

   with package Vectors is new Ada.Containers.Vectors (<>);
   --  Type of vectors that we want to iterate over

   with package Iterators is new Iters.Iterators (Vectors.Element_Type);
   --  Type of iterators in wich the vector will be wrapped

package Iters.Vec_Iterators is

   type Vec_Iterator is new Iterators.Iterator_Interface with private;
   --  Iterator that yields the values contained in a vector

   overriding function Next (Iter   : in out Vec_Iterator;
                             Result : out Vectors.Element_Type) return Boolean;
   --  Get the next iteration element. If all the values from the vector have
   --  already been yielded, return false. Otherwise, return true and set
   --  Result.

   overriding function Clone (Iter : Vec_Iterator) return Vec_Iterator;
   --  Make a deep copy of the iterator

   overriding procedure Release (Iter : in out Vec_Iterator);
   --  Release ressources that belong to Iter

   function To_Iterator (Vec : Vectors.Vector) return Vec_Iterator;
   --  Create a Vec_Iterator that wrapps 'Vec'

private

   type Vec_Access is access all Vectors.Vector;

   procedure Free_Vec_Access is new Ada.Unchecked_Deallocation
     (Vectors.Vector, Vec_Access);

   type Vec_Iterator is new Iterators.Iterator_Interface with record
      Elements : Vec_Access;
      Next_Element_Index : Vectors.Extended_Index;
   end record;

end Iters.Vec_Iterators;
