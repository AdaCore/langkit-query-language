------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with LKQL.Node_Data;
with LKQL.Error_Handling; use LKQL.Error_Handling;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with LKQL.Patterns.Nodes; use LKQL.Patterns.Nodes;
with LKQL.Selector_Lists; use LKQL.Selector_Lists;

with Ada.Assertions;                  use Ada.Assertions;

package body LKQL.Chained_Pattern is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Chained_Pattern_Iterator;
                             Result : out Match_Result)
                             return Boolean
   is
      Current_Root : H.AST_Node_Holder;
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
   is
   begin
      Inc_Ref (Iter.Ctx.Frames);
      return
        (Ctx                    => Iter.Ctx,
         Next_Values            => Iter.Next_Values,
         Pattern                => Iter.Pattern,
         Root_Nodes_Iterator =>
           new AST_Node_Iterator'Class'
             (AST_Node_Iterator'Class
               ((Iter.Root_Nodes_Iterator.Clone))),
         Yielded_Elements       => Node_Sets.Empty_Set);
   end Clone;

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

   function Make_Chained_Pattern_Iterator
     (Ctx           : Eval_Context;
      Root_Iterator : AST_Node_Iterator_Access;
      Pattern       : L.Chained_Node_Pattern) return Chained_Pattern_Iterator
   is
   begin
      Inc_Ref (Ctx.Frames);
      return (Ctx                    => Ctx,
              Pattern                => Pattern,
              Root_Nodes_Iterator    => Root_Iterator,
              others => <>);
   end Make_Chained_Pattern_Iterator;

   ------------------
   -- Eval_Element --
   ------------------

   procedure Eval_Element (Iter : in out Chained_Pattern_Iterator;
                           Root : H.AST_Node_Holder)
   is
      Match : constant Match_Result :=
        Match_Pattern
          (Iter.Ctx,
           Iter.Pattern.F_First_Pattern,
           To_Primitive (Root, Iter.Ctx.Pool));
   begin
      if not Match.Is_Success then
         return;
      end if;

      Iter.Eval_Chain_From (Root, Link_Nb => 1);
   end Eval_Element;

   ---------------------
   -- Eval_Chain_From --
   ---------------------

   procedure Eval_Chain_From (Iter        : in out Chained_Pattern_Iterator;
                              Root        : H.AST_Node_Holder;
                              Link_Nb     : Positive)
   is
   begin
      if Link_Nb > Iter.Pattern.F_Chain.Children_Count and then
        not (Iter.Yielded_Elements.Contains (Root))
      then
         Iter.Next_Values.Append
           (Make_Match_Success (To_Primitive (Root, Iter.Ctx.Pool)));
         Iter.Yielded_Elements.Insert (Root);
      elsif Link_Nb <= Iter.Pattern.F_Chain.Children_Count then
         Iter.Eval_Chain_From_Link (Root, Link_Nb);
      end if;
   end Eval_Chain_From;

   --------------------------
   -- Eval_Chain_From_Link --
   --------------------------

   procedure Eval_Chain_From_Link
     (Iter        : in out Chained_Pattern_Iterator;
      Root        : H.AST_Node_Holder;
      Link_Nb     : Positive)
   is
      Link             : constant L.Chained_Pattern_Link :=
        Iter.Pattern.F_Chain.List_Child (Link_Nb);
      Nodes            : constant AST_Node_Vector :=
        Eval_Link (Iter.Ctx, Root, Link);
      Pattern_Binding  : constant Symbol_Type :=
        Symbol (Link.F_Pattern.P_Binding_Name);
   begin
      if Nodes.Is_Empty then
         return;
      end if;

      for E of Nodes loop
         if Pattern_Binding /= null then
            Iter.Ctx.Add_Binding (Pattern_Binding,
                                  To_Primitive (E, Iter.Ctx.Pool));
         end if;

         Eval_Chain_From (Iter, E, Link_Nb + 1);
      end loop;
   end Eval_Chain_From_Link;

   ---------------------
   -- Eval_Chain_Link --
   ---------------------

   function Eval_Link (Ctx             : Eval_Context;
                       Root            : H.AST_Node_Holder;
                       Link            : L.Chained_Pattern_Link)
                       return AST_Node_Vector
   is
      Pattern : L.Base_Pattern renames Link.F_Pattern;
   begin
      case Link.Kind is
         when LCO.LKQL_Selector_Link =>
            return Eval_Selector_Link
              (Ctx, Root, Link.As_Selector_Link);
         when LCO.LKQL_Field_Link =>
            return Filter_Node_Vector
              (Ctx, Pattern,
               Eval_Field_Link (Ctx, Root, Link.As_Field_Link));
         when LCO.LKQL_Property_Link =>
            return Filter_Node_Vector
              (Ctx, Pattern,
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
                                Root            : H.AST_Node_Holder;
                                Selector        : L.Selector_Link)
                                return AST_Node_Vector
   is
      S_List       : Selector_List;
      Call         : constant L.Selector_Call := Selector.F_Selector;
      Binding_Name : constant Symbol_Type := Symbol (Call.P_Binding_Name);
   begin
      if not Eval_Selector
        (Ctx, Root, Call, Selector.F_Pattern, S_List)
      then
         return AST_Node_Vectors.Empty_Vector;
      end if;

      if Binding_Name /= null then
         Ctx.Add_Binding (Binding_Name, To_Primitive (S_List.Clone, Ctx.Pool));
      end if;

      return S_List.Nodes;
   end Eval_Selector_Link;

   ---------------------
   -- Eval_Field_Link --
   ---------------------

   function Eval_Field_Link (Ctx   : Eval_Context;
                             Root  : H.AST_Node_Holder;
                             Field : L.Field_Link)
                             return AST_Node_Vector
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

      return To_AST_Node_Vector (Field_Value);
   end Eval_Field_Link;

   ------------------------
   -- Eval_Property_Link --
   ------------------------

   function Eval_Property_Link (Ctx : Eval_Context;
                                Root : H.AST_Node_Holder;
                                Property : L.Property_Link)
                                return AST_Node_Vector
   is
      use LKQL.Node_Data;
      Call        : constant L.Fun_Call := Property.F_Property;
      Property_Value : constant Primitive :=
        Eval_Node_Property
          (Ctx, Root, Call.F_Name.As_Identifier, Call.F_Arguments);
   begin
      if Kind (Property_Value) /= Kind_Node
        and then Kind (Property_Value) /= Kind_List
      then
         Raise_Invalid_Kind
           (Ctx, Property.As_LKQL_Node, Kind_List, Property_Value);
      end if;

      return To_AST_Node_Vector (Property_Value);
   end Eval_Property_Link;

   -----------------------
   -- To_Ada_Node_Array --
   -----------------------

   function To_AST_Node_Vector (Value : Primitive) return AST_Node_Vector is
      Result : AST_Node_Vector;
   begin
      case Kind (Value) is
         when Kind_Node =>
            Result.Append (Node_Val (Value));
         when Kind_List =>
            for I in 1 .. Length (Value) loop
               Result.Append (Node_Val (Get (Value, I)));
            end loop;
         when others =>
            raise Assertion_Error with
              "Cannot make an ada node array from a value of kind: " &
              Kind_Name (Value);
      end case;
      return Result;
   end To_AST_Node_Vector;

end LKQL.Chained_Pattern;
