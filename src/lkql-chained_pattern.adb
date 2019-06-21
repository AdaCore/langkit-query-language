with LKQL.Node_Data;
with LKQL.Error_Handling; use LKQL.Error_Handling;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with LKQL.Patterns.Nodes; use LKQL.Patterns.Nodes;
with LKQL.Selector_Lists; use LKQL.Selector_Lists;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Assertions;                  use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body LKQL.Chained_Pattern is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Chained_Pattern_Iterator;
                             Result : out Match_Result)
                             return Boolean
   is
      Current_Root : AST_Node_Rc;
   begin
      while Iter.Next_Values.Is_Empty and then
        Iter.Root_Nodes_Iterator.Next (Current_Root)
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
       Root_Nodes_Iterator =>
          new AST_Node_Iterator'Class'
         (AST_Node_Iterator'Class ((Iter.Root_Nodes_Iterator.Clone))),
       Yielded_Elements       => Node_Sets.Empty_Set);

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Chained_Pattern_Iterator) is
   begin
      Iter.Ctx.Release_Current_Frame;
      AST_Node_Iterators.Free_Iterator (Iter.Root_Nodes_Iterator);
   end Release;

   -------------------------------
   -- Make_Chained_Pattern_Iter --
   -------------------------------

   function Make_Chained_Pattern_Iterator (Ctx     : Eval_Context;
                                           Pattern : L.Chained_Node_Pattern)
                                           return Chained_Pattern_Iterator
   is
      Root_Iterator : constant AST_Node_Iterator_Access :=
        new AST_Node_Iterator'Class'
          (AST_Node_Iterator'Class (Make_Child_Iterator (Ctx.AST_Root)));
   begin
      return (Ctx                    => Ctx.Clone_Frame,
              Pattern                => Pattern,
              Root_Nodes_Iterator    => Root_Iterator,
              others => <>);
   end Make_Chained_Pattern_Iterator;

   ------------------
   -- Eval_Element --
   ------------------

   procedure Eval_Element (Iter : in out Chained_Pattern_Iterator;
                           Root : AST_Node_Rc)
   is
      Match : constant Match_Result :=
        Match_Unfiltered
          (Iter.Ctx, Iter.Pattern.F_First_Pattern, To_Primitive (Root));
   begin
      if not Match.Is_Success then
         return;
      end if;

      Iter.Eval_Chain_From (Root, Match.Bindings, Link_Nb => 1);
   end Eval_Element;

   ---------------------
   -- Eval_Chain_From --
   ---------------------

   procedure Eval_Chain_From (Iter        : in out Chained_Pattern_Iterator;
                              Root        : AST_Node_Rc;
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
      Root        : AST_Node_Rc;
      Current_Env : Environment_Map;
      Link_Nb     : Positive)
   is
      Env              : Environment_Map := Current_Env;
      Link             : constant L.Chained_Pattern_Link :=
        Iter.Pattern.F_Chain.List_Child (Link_Nb);
      Pattern          : constant L.Unfiltered_Pattern := Link.F_Pattern;
      Nodes            : constant AST_Node_Rc_Array :=
        Eval_Link (Iter.Ctx, Root, Link, Pattern, Env);
      Pattern_Binding  : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Pattern.P_Binding_Name);
   begin
      if Nodes'Length = 0 then
         return;
      end if;

      for E of Nodes loop
         if Length (Pattern_Binding) /= 0 then
            Env.Include (Pattern_Binding, To_Primitive (E));
         end if;

         Eval_Chain_From (Iter, E, Env, Link_Nb + 1);
      end loop;
   end Eval_Chain_From_Link;

   ---------------------
   -- Eval_Chain_Link --
   ---------------------

   function Eval_Link (Ctx             : Eval_Context;
                       Root            : AST_Node_Rc;
                       Link            : L.Chained_Pattern_Link;
                       Related_Pattern : L.Unfiltered_Pattern;
                       Bindings        : in out Environment_Map)
                       return AST_Node_Rc_Array
   is
   begin
      case Link.Kind is
         when LCO.LKQL_Selector_Link =>
            return Eval_Selector_Link
              (Ctx, Root, Link.As_Selector_Link, Related_Pattern, Bindings);
         when LCO.LKQL_Field_Link =>
            return Filter_Node_Array
              (Ctx, Related_Pattern.As_Base_Pattern,
               Eval_Field_Link (Ctx, Root, Link.As_Field_Link));
         when LCO.LKQL_Property_Link =>
            return Filter_Node_Array
              (Ctx, Related_Pattern.As_Base_Pattern,
               Eval_Property_Link (Ctx, Root, Link.As_Property_Link));
         when others =>
            raise Assertion_Error with
              "Invalid chained pattern link kind: " & L.Kind_Name (Link);
      end case;
   end Eval_Link;

   ------------------------
   -- Eval_Selector_Link --
   ------------------------

   function Eval_Selector_Link (Ctx             : Eval_Context;
                                Root            : AST_Node_Rc;
                                Selector        : L.Selector_Link;
                                Related_Pattern : L.Unfiltered_Pattern;
                                Bindings        : in out Environment_Map)
                                return AST_Node_Rc_Array
   is
      S_List       : Selector_List;
      Call         : constant L.Selector_Call := Selector.F_Selector;
      Binding_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Call.P_Binding_Name);
      Empty_Array  : AST_Node_Rc_Array (1 .. 0);
   begin
      if not Eval_Selector
        (Ctx, Root, Call, Related_Pattern.As_Base_Pattern, S_List)
      then
         return Empty_Array;
      end if;

      if Length (Binding_Name) /= 0 then
         Bindings.Insert (Binding_Name, To_Primitive (S_List.Clone));
      end if;

      return S_List.Nodes;
   end Eval_Selector_Link;

   ---------------------
   -- Eval_Field_Link --
   ---------------------

   function Eval_Field_Link (Ctx   : Eval_Context;
                             Root  : AST_Node_Rc;
                             Field : L.Field_Link)
                             return AST_Node_Rc_Array
   is
      use LKQL.Node_Data;
      Field_Value : constant Primitive :=
        Access_Node_Field (Ctx, Root, Field.F_Field);
   begin
      if Kind (Field_Value) /= Kind_Node
        and then Kind (Field_Value) /= Kind_List
      then
         Raise_Invalid_Kind
           (Ctx, Field.As_LKQL_Node, Kind_List, Field_Value);
      end if;

      return To_Ada_Node_Array (Field_Value);
   end Eval_Field_Link;

   ------------------------
   -- Eval_Property_Link --
   ------------------------

   function Eval_Property_Link (Ctx : Eval_Context;
                                Root : AST_Node_Rc;
                                Property : L.Property_Link)
                                return AST_Node_Rc_Array
   is
      use LKQL.Node_Data;
      Call        : constant L.Fun_Call := Property.F_Property;
      Property_Value : constant Primitive :=
        Eval_Node_Property (Ctx, Root, Call.F_Name, Call.F_Arguments);
   begin
      if Kind (Property_Value) /= Kind_Node
        and then Kind (Property_Value) /= Kind_List
      then
         Raise_Invalid_Kind
           (Ctx, Property.As_LKQL_Node, Kind_List, Property_Value);
      end if;

      return To_Ada_Node_Array (Property_Value);
   end Eval_Property_Link;

   -----------------------
   -- To_Ada_Node_Array --
   -----------------------

   function To_Ada_Node_Array (Value : Primitive) return AST_Node_Rc_Array is
   begin
      case Kind (Value) is
         when Kind_Node =>
            return Result : AST_Node_Rc_Array (1 .. 1) do
               Result (1) := Node_Val (Value);
            end return;

         when Kind_List =>
            return Result : AST_Node_Rc_Array (1 .. Length (Value)) do
               for I in 1 .. Length (Value) loop
                  Result (I) := Node_Val (Get (Value, I));
               end loop;
            end return;

         when others =>
            raise Assertion_Error with
              "Cannot make an ada node array from a value of kind: " &
              Kind_Name (Value);
      end case;
   end To_Ada_Node_Array;

end LKQL.Chained_Pattern;
