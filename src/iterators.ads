with Libadalang.Iterators; use Libadalang.Iterators;
with Libadalang.Analysis;  use Libadalang.Analysis;

package Iterators is

   generic
      type Iter_Type (<>) is new Ada_Node_Iterators.Iterator with private;
      --  Type of the iterator from with the AST nodes will be drawn

   package Filters is

      type Iter_Type_Access is access all Iter_Type;

      type Filter_Iter is new Ada_Node_Iterators.Iterator with private;
      --  Iterator adapter that filters the elements of the wrapped iterator

      procedure Free_Filter (Self : in out Filter_Iter);

      function Filter (Iter      : Iter_Type;
                       Predicate : Ada_Node_Predicate) return Filter_Iter;
      --  Return an iterator over Iter's elements that uses Predicate as a
      --  filtering function.
      --  The ewly created iterator will create a copy of Iter and consume it's
      --  content.

      function Filter (Iter      : Iter_Type_Access;
                       Predicate : Ada_Node_Predicate) return Filter_Iter;
      --  Return an iterator over Iter's elements that uses Predicate as a
      --  filtering function.
      --  The newly created iterator will take ownership of the Iter pointer

      function Next
        (I : in out Filter_Iter; Element : out Ada_Node) return Boolean;
   private

      type Filter_Iter is new Ada_Node_Iterators.Iterator with record
         Iter      : Iter_Type_Access;
         Predicate : Ada_Node_Predicate;
      end record;

   end Filters;

end Iterators;
