
package body Iters.Iterators is

   -------------
   -- Consume --
   -------------

   function Consume (Iter : Iterator_Interface'Class) return Element_Array
   is
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

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Filter_Iter) return Filter_Iter
   is
      Predicate_Copy : constant Predicates.Func_Access :=
        new Predicates.Func'Class'
          (Predicates.Func'Class (Iter.Predicate.Clone));
      Inner_Copy     : constant Iterator_Access :=
        new Iterator_Interface'Class'
          (Iterator_Interface'Class (Iter.Inner.Clone));
   begin
      return Filter_Iter'(Inner_Copy, Predicate_Copy);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Filter_Iter) is
   begin
      Iter.Inner.Release;
      Free_Iterator (Iter.Inner);
      Iter.Predicate.Release;
      Predicates.Free_Func (Iter.Predicate);
   end Release;

   ------------
   -- Filter --
   ------------

   function Filter
     (Iter : Iterator_Access; Pred : Predicate_Access) return Filter_Iter
   is
   begin
      return (Iter, Pred);
   end Filter;

   ------------
   -- Filter --
   ------------

   function Filter (Iter : Iterator_Interface'Class;
                    Pred : Predicates.Func'Class)
                    return Filter_Iter
   is
      Iter_Ptr : constant Iterator_Access :=
        new Iterator_Interface'Class'(Iter);
      Pred_Ptr : constant Predicate_Access :=
        new Predicates.Func'Class'(Pred);
   begin
      return Filter (Iter_Ptr, Pred_Ptr);
   end Filter;

   function Read_From_Inner (Iter   : in out Resetable_Iter;
                             Result : out Element_Type) return Boolean;
   --  Read a value from the resetable iterator's inner iterator

   function Read_From_Cache (Iter   : in out Resetable_Iter;
                             Result : out Element_Type) return Boolean;
   --  Read a value from the resetable iterator's cache

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Resetable_Iter;
                             Result : out Element_Type) return Boolean
   is
   begin
      return (if Iter.Cache_Pos = -1
              then Read_From_Inner (Iter, Result)
              else Read_From_Cache (Iter, Result));
   end Next;

   ---------------------
   -- Read_From_Inner --
   ---------------------

   function Read_From_Inner (Iter   : in out Resetable_Iter;
                             Result : out Element_Type) return Boolean
   is
   begin
      if Iter.Inner.Next (Result) then
         Iter.Cache.Append (Result);
         return True;
      else
         return False;
      end if;
   end Read_From_Inner;

   ---------------------
   -- Read_From_Cache --
   ---------------------

   function Read_From_Cache (Iter   : in out Resetable_Iter;
                             Result : out Element_Type) return Boolean
   is
   begin
      if Iter.Cache.Is_Empty or Iter.Cache_Pos > Iter.Cache.Last_Index then
         return False;
      end if;

      Result := Iter.Cache.all (Iter.Cache_Pos);
      Iter.Cache_Pos := Iter.Cache_Pos + 1;
      return True;
   end Read_From_Cache;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Resetable_Iter) is
   begin
      Free_Element_Vector (Iter.Cache);
      Iter.Release;
      Free_Iterator (Iter.Inner);
   end Release;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Resetable_Iter) return Resetable_Iter is
      Inner_Copy : constant Iterator_Access :=
        new Iterator_Interface'Class'
          (Iterator_Interface'Class (Iter.Inner.Clone));
      Cache_Copy : constant Element_Vector_Access :=
        new Element_Vectors.Vector'(Iter.Cache.all);
   begin
      return (Inner_Copy, Cache_Copy, Iter.Cache_Pos);
   end Clone;

   -----------
   -- Reset --
   -----------

   procedure Reset (Iter : in out Resetable_Iter) is
   begin
      Iter.Cache_Pos := 1;
   end Reset;

   ---------------
   -- Resetable --
   ---------------

   function Resetable (Iter : Iterator_Interface'Class) return Resetable_Iter
   is (Resetable_Iter'
         (Inner => new Iterator_Interface'Class'(Iter), others => <>));

   ---------------
   -- Resetable --
   ---------------

   function Resetable (Iter : Iterator_Access) return Resetable_Iter is
      (Resetable_Iter'(Inner => Iter, others => <>));

   -----------------
   -- To_Iterator --
   -----------------

   function To_Iterator
     (Elements : Element_Vectors.Vector) return Vec_Iterator
   is
      Elements_Copy : constant Element_Vector_Access :=
        new Element_Vectors.Vector'(Elements);
   begin
      return Vec_Iterator'(Elements_Copy, Positive'First);
   end To_Iterator;

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Vec_Iterator;
                             Result : out Element_Type) return Boolean is
   begin
      if Iter.Next_Element > Iter.Elements.Last_Index then
         return False;
      end if;

      Result := Iter.Elements.all (Iter.Next_Element);
      Iter.Next_Element := Iter.Next_Element + 1;
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Vec_Iterator) return Vec_Iterator is
      Elements_Copy : constant Element_Vector_Access :=
        new Element_Vectors.Vector'(Iter.Elements.all);
   begin
      return Vec_Iterator'(Elements_Copy, Iter.Next_Element);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Vec_Iterator) is
   begin
      Free_Element_Vector (Iter.Elements);
   end Release;

end Iters.Iterators;
