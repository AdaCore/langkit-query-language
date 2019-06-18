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

   ----------------------
   -- Make_Ast_Node_Rc --
   ----------------------

   function Make_AST_Node_Rc
     (Node : in out AST_Node_Access) return AST_Node_Rc
   is
      Copy_Rc : constant AST_Node_Rc := Make_AST_Node_Rc (Node.all);
   begin
      Free_AST_Node (Node);
      return Copy_Rc;
   end Make_AST_Node_Rc;

   ----------------------------
   -- Release_AST_Node_Array --
   ----------------------------

   procedure Release_AST_Node_Array (Value : in out AST_Node_Array_Access) is
   begin
      for N of Value.all loop
         Free_AST_Node (N);
      end loop;

      Free_Ast_Node_Array (Value);
   end Release_AST_Node_Array;

   ---------------------------------
   -- Release_Introspection_Value --
   ---------------------------------

   procedure Release_Introspection_Value (Value : in out Introspection_Value)
   is
   begin
      case Value.Kind is
         when Kind_Node_Array =>
            Release_AST_Node_Array (Value.Node_Array_Val);
         when Kind_Text_Array =>
            Free_Unbounded_Text_Array (Value.Text_Array_Val);
         when others => null;
      end case;
   end Release_Introspection_Value;

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
