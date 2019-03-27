with Libadalang.Analysis; use Libadalang.Analysis;

package body Query.Iterators is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter : in out Childs_Iterator;
                             Element : out Iterator_Node)
                             return Boolean
   is
      Result : constant Option := Pop (Iter.Stack);
   begin
      if Is_None (Result) then
         return False;
      end if;

      Element := Extract (Result);
      Stack_Childs (Iter, Element);
      return True;
   end Next;

   --------------------------
   -- Make_Childs_Iterator --
   --------------------------

   function Make_Childs_Iterator (Node : LAL.Ada_Node) return Childs_Iterator
   is
   begin
      return Result : Childs_Iterator do
         Result.Stack := new Iterator_Node_Vectors.Vector;
         Result.Stack.Append ((Node, 0));
      end return;
   end Make_Childs_Iterator;

   ------------------
   -- Stack_Childs --
   ------------------

   procedure Stack_Childs
     (Iter : in out Childs_Iterator; Element : Iterator_Node)
   is
      Children : Ada_Node_Array renames Element.Node.Children;
   begin
      for I in reverse Children'Range loop
         if not Children (I).Is_Null then
            Iter.Stack.Append ((Children (I), Element.Depth + 1));
         end if;
      end loop;
   end Stack_Childs;

   ---------
   -- Pop --
   ---------

   function Pop
     (Stack  : Element_Vector_Access) return Option
   is
      use Iterator_Node_Vectors;
      Result        : Iterator_Node;
      Result_Cursor : constant Cursor := Stack.all.Last;
   begin
      if not Has_Element (Result_Cursor) then
         return None;
      end if;

      Result := Element (Result_Cursor);
      Stack.all.Delete_Last;
      return To_Option (Result);
   end Pop;

   -------------
   -- Release --
   -------------

   procedure Release (Iter : in out Childs_Iterator) is
   begin
      Free_Element_Vector (Iter.Stack);
   end Release;

end Query.Iterators;
