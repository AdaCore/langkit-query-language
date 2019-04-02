with Funcs;
with Iters.Iterators;

generic

   with package Input_Iterators is new Iters.Iterators (<>);
   --  Wrapped iterators

   with package Output_Iterators is new Iters.Iterators (<>);
   --  Wrapped iterators

package Iters.Maps is

   subtype Result_Type is Output_Iterators.Element_Type;

   package Map_Funcs is new Funcs (Input_Iterators.Element_Type, Result_Type);
   --  Abstraction representing a function that takes values from the input
   --  iterator and returns values of type Return_Type.

   subtype Map_Func is Map_Funcs.Func;

   package Predicates renames Output_Iterators.Predicates;

   type Map_Iter is new Output_Iterators.Iterator_Interface with private;
   --  Iterator that maps a function over the elements of a given iterator

   overriding function Next (Iter   : in out Map_Iter;
                             Result : out Result_Type) return Boolean;

   overriding function Clone (Iter : Map_Iter) return Map_Iter;

   overriding procedure Release (Iter : in out Map_Iter);

   function Map (Input : Input_Iterators.Iterator_Access;
                 Fn    : Map_Funcs.Func_Access) return Map_Iter;

   function Map (Input : Input_Iterators.Iterator_Interface'Class;
                 Fn    : Map_Funcs.Func'Class) return Map_Iter;

private

   type Map_Iter is new Output_Iterators.Iterator_Interface with record
      Inner : Input_Iterators.Iterator_Access;
      Fn    : Map_Funcs.Func_Access;
   end record;

end Iters.Maps;
