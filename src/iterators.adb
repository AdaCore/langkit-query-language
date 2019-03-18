with Ada.Unchecked_Deallocation;

package body Iterators is

   package body Filters is

      procedure Free_Ada_Node_Iterator is new Ada.Unchecked_Deallocation
        (Iter_Type, Iter_Type_Access);

      procedure Free_Filter (Self : in out Filter_Iter) is
      begin
         Free_Ada_Node_Iterator (Self.Iter);
      end Free_Filter;

      ------------
      -- Filter --
      ------------

      function Filter (Iter      : Iter_Type;
                       Predicate : Ada_Node_Predicate) return Filter_Iter
      is
      begin
         return (new Iter_Type'(Iter), Predicate);
      end Filter;

      ------------
      -- Filter --
      ------------

      function Filter (Iter      : Iter_Type_Access;
                       Predicate : Ada_Node_Predicate) return Filter_Iter
      is
      begin
         return (Iter, Predicate);
      end Filter;

      ----------
      -- Next --
      ----------

      function Next
        (I       : in out  Filter_Iter;
         Element : out Ada_Node) return Boolean
      is
         Current_Node : Ada_Node;
      begin
         while I.Iter.Next (Current_Node) loop
            if I.Predicate.Get.Evaluate (Current_Node) then
               Element := Current_Node;
               return True;
            end if;
         end loop;

         return False;
      end Next;

   end Filters;

end Iterators;
