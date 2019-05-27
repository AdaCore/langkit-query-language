with LKQL.Queries;        use LKQL.Queries;
with LKQL.Primitives;     use LKQL.Primitives;
with LKQL.Patterns.Nodes; use LKQL.Patterns.Nodes;
with LKQL.Selector_Lists; use LKQL.Selector_Lists;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body LKQL.Chained_Pattern is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Chained_Pattern_Iterator;
                             Result : out Match_Result)
                             return Boolean
   is
      Current_Root : LAL.Ada_Node;
   begin
      while Iter.Next_Values.Is_Empty and then
        Iter.Root_Elements_Iterator.Next (Current_Root)
      loop
         Iter.Eval_Element (Current_Root);
      end loop;

      if Iter.Next_Values.Is_Empty then
         return False;
      else
         Result := Iter.Next_Values.First_Element;
         Iter.Next_Values.Delete_First;
         return True;
      end if;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Chained_Pattern_Iterator)
                              return Chained_Pattern_Iterator
   is (Ctx                    => Iter.Ctx.Clone_Frame,
       Next_Values            => Iter.Next_Values,
       Pattern                => Iter.Pattern,
       Root_Elements_Iterator =>
          new Node_Iterator'Class'
         (Node_Iterator'Class ((Iter.Root_Elements_Iterator.Clone))),
       Yielded_Elements       => Node_Sets.Empty_Set);

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Chained_Pattern_Iterator) is
   begin
      Iter.Ctx.Release_Current_Frame;
      Node_Iterators.Free_Iterator (Iter.Root_Elements_Iterator);
   end Release;

   -------------------------------
   -- Make_Chained_Pattern_Iter --
   -------------------------------

   function Make_Chained_Pattern_Iterator (Ctx     : Eval_Context;
                                           Pattern : L.Chained_Node_Pattern)
                                           return Chained_Pattern_Iterator
   is
      Root_Iterator : constant Node_Iterator_Access :=
        new Node_Iterator'Class'
          (Make_Query_Iterator (Ctx, Pattern.F_First_Pattern.As_Base_Pattern));
   begin
      return (Ctx                    => Ctx.Clone_Frame,
              Pattern                => Pattern,
              Root_Elements_Iterator => Root_Iterator,
              others => <>);
   end Make_Chained_Pattern_Iterator;

   ------------------
   -- Eval_Element --
   ------------------

   procedure Eval_Element (Iter : in out Chained_Pattern_Iterator;
                           Root : LAL.Ada_Node)
   is
      Env          : Environment_Map;
      Root_Binding : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Iter.Pattern.F_First_Pattern.P_Binding_Name);
   begin
      if Length (Root_Binding) /= 0 then
         Env.Insert (Root_Binding, To_Primitive (Root));
      end if;

      Iter.Eval_Chain_From (Root, Env, Link_Nb => 1);
   end Eval_Element;

   ---------------------
   -- Eval_Chain_From --
   ---------------------

   procedure Eval_Chain_From (Iter        : in out Chained_Pattern_Iterator;
                              Root        : LAL.Ada_Node;
                              Current_Env : Environment_Map;
                              Link_Nb     : Positive)
   is
   begin
      if Link_Nb > Iter.Pattern.F_Chain.Children_Count and then
        not (Iter.Yielded_Elements.Contains (Root))
      then
         Iter.Next_Values.Append
           (Make_Match_Success (To_Primitive (Root), Current_Env));
         Iter.Yielded_Elements.Insert (Root);
      elsif Link_Nb <= Iter.Pattern.F_Chain.Children_Count then
         Iter.Eval_Chain_From_Link (Root, Current_Env, Link_Nb);
      end if;
   end Eval_Chain_From;

   --------------------------
   -- Eval_Chain_From_Link --
   --------------------------

   procedure Eval_Chain_From_Link
     (Iter        : in out Chained_Pattern_Iterator;
      Root        : LAL.Ada_Node;
      Current_Env : Environment_Map;
      Link_Nb     : Positive)
   is
      Elements         : Selector_List;
      Env              : Environment_Map := Current_Env;
      Link             : constant L.Chained_Pattern_Link :=
        Iter.Pattern.F_Chain.List_Child (Link_Nb);
      Selector_Call    : constant L.Selector_Call := Link.F_Selector;
      Pattern          : constant L.Base_Pattern :=
        Link.F_Pattern.As_Base_Pattern;
      Selector_Binding : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Selector_Call.P_Binding_Name);
      Pattern_Binding  : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Pattern.P_Binding_Name);
   begin
      if not Eval_Selector (Iter.Ctx, Root, Selector_Call, Pattern, Elements)
      then
         return;
      end if;

      if Length (Selector_Binding) /= 0 then
         Env.Insert (Selector_Binding, To_Primitive (Elements.Clone));
      end if;

      for E of Elements.Nodes loop
         if Length (Pattern_Binding) /= 0 then
            Env.Insert (Pattern_Binding, To_Primitive (E));
         end if;

         Eval_Chain_From (Iter, E, Env, Link_Nb + 1);
      end loop;
   end Eval_Chain_From_Link;

end LKQL.Chained_Pattern;
