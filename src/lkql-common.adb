package body LKQL.Common is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter : in out Childs_Iterator;
                             Result : out AST_Node_Rc) return Boolean
   is
     (Iter.Inner.Next (Result));

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Childs_Iterator) return Childs_Iterator is
     (Make_Childs_Iterator (Iter.Root));

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Childs_Iterator) is
   begin
      Free_Traverse_Iterator (Iter.Inner);
   end Release;

   --------------------------
   -- Make_Childs_Iterator --
   --------------------------

   function Make_Childs_Iterator (Root : AST_Node_Rc) return Childs_Iterator
   is
      Inner : constant Traverse_Iterator_Access :=
        new Traverse_Iterator'Class'(Traverse (Root));
   begin
      return Childs_Iterator'(Inner, Root);
   end Make_Childs_Iterator;

end LKQL.Common;
