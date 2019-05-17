with Funcs;
with Options;

with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;

generic

   type Element_Type is private;
   --  Type of the values yielded by the iterator

package Iters.Iterators is

   type Element_Array is array (Positive range <>) of Element_Type;
   --  Array type to use when consuming the iterator into an array of elements

   package Element_Vectors is new Ada.Containers.Vectors
     (Positive, Element_Type);
   --  Vectors of Element_Type values

   type Element_Vector_Access is access all Element_Vectors.Vector;
   --  Pointer to a vector of Element_Type values

   package Element_Options is new Options (Element_Type);
   --  Optionnal Element_Type values

   subtype Element_Option is Element_Options.Option;
   --  Optionnal Element_Type value

   --------------
   -- Iterator --
   --------------

   type Iterator_Interface is interface;
   --  Abstraction for iterating over a sequence of values

   type Iterator_Access is access all Iterator_Interface'Class;

   function Next (Iter   : in out Iterator_Interface;
                  Result : out Element_Type) return Boolean is abstract;
   --  Get the next iteration element. If there was no element to yield
   --  anymore, return false. Otherwise, return true and set Result.

   function Clone
     (Iter : Iterator_Interface) return Iterator_Interface is abstract;
   --  Make a deep copy of the iterator

   procedure Release (Iter : in out Iterator_Interface) is null;
   --  Release ressources that belong to Iter

   function Consume (Iter : Iterator_Interface'Class) return Element_Array;
   --  Consume the iterator and return an array that contains the yielded
   --  elements.
   --  The ressources associated with the iterator will be released.

   procedure Release_Access (Iter : in out Iterator_Access);
   --  Release the ressources that belong to the iterator accessed trough
   --  'Iter', then free 'Iter' itself.

   procedure Free_Iterator is new Ada.Unchecked_Deallocation
     (Iterator_Interface'Class, Iterator_Access);

   ---------------
   -- Consumers --
   ---------------

   generic

      type Return_Type (<>) is private;
      --  Type of values produced by the iterator's consumption

   package Consumers is

      type Consumer_Interface is interface;

      function Consume (Self : in out Consumer_Interface;
                        Iter : in out Iterator_Interface'Class)
                        return Return_Type is abstract;
      --  Consume and release the iterator

   end Consumers;

   ------------
   -- Filter --
   ------------

   package Predicates is new Funcs (Element_Type, Boolean);
   --  Function that takes an Element_Type argument an retrurns a Boolean

   subtype Predicate_Access is Predicates.Func_Access;
   --  Pointer to a predicate

   type Filter_Iter is new Iterator_Interface with private;
   --  Iterator that wraps an other iterator an filters it's elements using a
   --  Predicate.

   function Filtered_Count (Iter : Filter_Iter) return Natural;
   --  Return the number of filtered elements

   overriding function Next (Iter   : in out Filter_Iter;
                             Result : out Element_Type) return Boolean;

   overriding function Clone (Iter : Filter_Iter) return Filter_Iter;

   overriding procedure Release (Iter : in out Filter_Iter);

   function Filter
     (Iter : Iterator_Access; Pred : Predicate_Access) return Filter_Iter;

   function Filter (Iter : Iterator_Interface'Class;
                    Pred : Predicates.Func'Class)
                    return Filter_Iter;

   ---------------
   -- Resetable --
   ---------------

   type Resetable_Iter is new Iterator_Interface with private;
   --  Resetable iterator that caches the elements that it yields.

   type Resetable_Access is access all Resetable_Iter;

   overriding function Next (Iter   : in out Resetable_Iter;
                             Result : out Element_Type) return Boolean;

   overriding procedure Release (Iter : in out Resetable_Iter);

   overriding function Clone (Iter : Resetable_Iter) return Resetable_Iter;

   function Get_Cached
     (Iter : Resetable_Iter; Pos : Positive) return Element_Option;
   --  Get the cached element at Pos, if it exists

   function Cache_Length (Iter : Resetable_Iter) return Natural;
   --  Return the number of cached elements

   function Get_Inner (Iter : Resetable_Iter) return Iterator_Access;
   --  Return an access to the wrapped iterator

   procedure Reset (Iter : in out Resetable_Iter);
   --  Reset the iterator. Further calls to 'Next' will yield the cached
   --  elements.

   function Resetable (Iter : Iterator_Interface'Class) return Resetable_Iter;

   function Resetable (Iter : Iterator_Access) return Resetable_Iter;

private

   procedure Free_Predicate_Access is new Ada.Unchecked_Deallocation
     (Predicates.Func'Class, Predicate_Access);

   type Filter_Iter is new Iterator_Interface with record
      Inner       : Iterator_Access;
      Predicate   : Predicate_Access;
      Nb_Filtered : Natural := 0;
   end record;

   procedure Free_Element_Vector is new Ada.Unchecked_Deallocation
     (Element_Vectors.Vector, Element_Vector_Access);

   subtype Cache_Index is Integer range -1 .. Integer'Last;

   type Resetable_Iter is new Iterator_Interface with record
      Inner : Iterator_Access;
      --  Wrapped iterator
      Cache : Element_Vector_Access := new Element_Vectors.Vector;
      --  Cached values
      Cache_Pos : Cache_Index := -1;
      --  Index of the next cache value to read.
      --  -1 if the wrapped iterator hasn't been fully consumed yet
   end record;

end Iters.Iterators;
