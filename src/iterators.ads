with Ada.Unchecked_Deallocation;

generic

   type Element_Type is private;
   --  Type of the values yielded by the iterator

package Iterators is

   type Element_Array is array (Positive range <>) of Element_Type;

   generic

      type Return_Type (<>) is private;
      --  Closure's return type

   package Funcs is

      type Func is interface;
      --  Abstraction representing a function

      function Evaluate (Self    : in out Func;
                         Element : Element_Type)
                         return Return_Type is abstract;
      --  Apply Self to the given value

   end Funcs;

   type Iterator is interface;
   --  Abstraction for iterating over a sequence of values

   type Iterator_Access is access all Iterator'Class;

   function Next (Iter   : in out Iterator;
                  Result : out Element_Type) return Boolean is abstract;
   --  Get the next iteration element. If there was no element to yield
   --  anymore, return false. Otherwise, return true and set Result.

   procedure Release (Iter : in out Iterator) is null;
   --  Release ressources that belong to Iter

   function Consume (Iter : Iterator'Class) return Element_Array;
   --  Consume the iterator and return an array that contains the yielded
   --  elements.
   --  The ressources associated with the iterator will be released.

   package Predicates is new Funcs (Boolean);
   --  Function that takes an Element_Type argument an retrurns a Boolean

   type Predicate_Access is access all Predicates.Func'Class;

   type Filter_Iter is new Iterator with private;
   --  Iterator that wraps an other iterator an filters it's elements using a
   --  Predicate.

   overriding function Next (Iter   : in out Filter_Iter;
                             Result : out Element_Type) return Boolean;

   overriding procedure Release (Iter : in out Filter_Iter);

   function Filter
     (Iter : Iterator_Access; Pred : Predicate_Access) return Filter_Iter;

private

   procedure Free_Iter_Access is new Ada.Unchecked_Deallocation
     (Iterator'Class, Iterator_Access);

   procedure Free_Predicate_Access is new Ada.Unchecked_Deallocation
     (Predicates.Func'Class, Predicate_Access);

   type Filter_Iter is new Iterator with record
      Inner     : Iterator_Access;
      Predicate : Predicate_Access;
   end record;

end Iterators;
