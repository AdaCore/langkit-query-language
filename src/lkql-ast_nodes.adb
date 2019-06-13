pragma Ada_2012;
package body LKQL.AST_Nodes is

   ----------------------
   -- Make_AST_Node_Rc --
   ----------------------

   function Make_AST_Node_Rc (Node : AST_Node'Class) return AST_Node_Rc is
      Ref : AST_Node_Ptrs.Ref;
   begin
      Ref.Set (Node);
      return Ref;
   end Make_AST_Node_Rc;

   procedure Add_Children (Iter : in out Child_Iterator; Node : AST_Node_Rc);

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter : in out Child_Iterator; Result : out AST_Node_Rc) return Boolean
   is
   begin
      if Iter.Next_Elements.Is_Empty then
         return False;
      end if;

      Result := Iter.Next_Elements.First_Element;
      Iter.Next_Elements.Delete_First;
      Add_Children (Iter, Result);

      return True;
   end Next;

   ------------------
   -- Add_Children --
   ------------------

   procedure Add_Children (Iter : in out Child_Iterator; Node : AST_Node_Rc) is
   begin
      for I in 1 .. Node.Get.Children_Count loop
         Iter.Next_Elements.Append (Make_AST_Node_Rc (Node.Get.Nth_Child (I)));
      end loop;
   end Add_Children;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Child_Iterator) return Child_Iterator is
     (Make_Child_Iterator (Iter.Root));

   -------------------------
   -- Make_Child_Iterator --
   -------------------------

   function Make_Child_Iterator (Node : AST_Node_Rc) return Child_Iterator
   is
      Result : Child_Iterator := (Root => Node, others => <>);
   begin
      Add_Children (Result, Node);
      return Result;
   end Make_Child_Iterator;

end LKQL.AST_Nodes;
