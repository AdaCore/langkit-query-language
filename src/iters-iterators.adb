with Ada.Containers.Vectors;

package body Iters.Iterators is
   ----------
   -- Next --
   ----------

   function Next
     (Iter   : in out Filter_Iter; Result : out Element_Type) return Boolean
   is
      Current_Element : Element_Type;
   begin
      while Iter.Inner.Next (Current_Element) loop
         if Iter.Predicate.Evaluate (Current_Element) then
            Result := Current_Element;
            return True;
         end if;
      end loop;

      return False;
   end Next;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Filter_Iter) is
   begin
      Iter.Inner.Release;
      Free_Iterator (Iter.Inner);
      Predicates.Free_Func (Iter.Predicate);
   end Release;

   -------------
   -- Consume --
   -------------

   function Consume (Iter : Iterator_Interface'Class) return Element_Array
   is
      package Element_Vectors is new Ada.Containers.Vectors
        (Positive, Element_Type);
      Vec     : Element_Vectors.Vector;
      Element : Element_Type;
   begin
      --  Note: This is bad design in Ada: We're hiding mutation of the
      --  Iterator object, because if we make it mutable, then you can no
      --  longer call consume on an expression that returns an Iterator, which
      --  in user APIs is not very friendly, because it means you cannot write
      --  write::
      --
      --      for Element of Node.Find (...).Consume loop
      --         ...
      --      end loop;
      --
      --  You have to declare the iterator explicitly.

      while Iter'Unrestricted_Access.Next (Element) loop
         Vec.Append (Element);
      end loop;

      Iter'Unrestricted_Access.Release;

      return Result : Element_Array (1 .. Natural (Vec.Length)) do
         for I in Result'Range loop
            Result (I) := Vec.Element (I);
         end loop;
      end return;
   end Consume;

   ------------
   -- Filter --
   ------------

   function Filter
     (Iter : Iterator_Access; Pred : Predicate_Access) return Filter_Iter
   is
   begin
      return (Iter, Pred);
   end Filter;

end Iters.Iterators;
