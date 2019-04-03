
package body Iters.Vec_Iterators is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Vec_Iterator;
                             Result : out Vectors.Element_Type) return Boolean
   is
      Cursor : constant Vectors.Cursor :=
        Iter.Elements.To_Cursor (Iter.Next_Element_Index);
   begin
      if not Vectors.Has_Element (Cursor) then
         return False;
      end if;

      Result := Vectors.Element (Cursor);
      Iter.Next_Element_Index :=
        Vectors.Extended_Index'Succ (Iter.Next_Element_Index);
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Vec_Iterator) return Vec_Iterator is
      Elements_Copy : constant Vec_Access :=
        new Vectors.Vector'(Iter.Elements.all);
   begin
      return Vec_Iterator'(Elements_Copy, Iter.Next_Element_Index);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Vec_Iterator) is
   begin
      Free_Vec_Access (Iter.Elements);
   end Release;

   -----------------
   -- To_Iterator --
   -----------------

   function To_Iterator (Vec : Vectors.Vector) return Vec_Iterator is
      (Vec_Iterator'(new Vectors.Vector'(Vec), Vec.First_Index));

end Iters.Vec_Iterators;
