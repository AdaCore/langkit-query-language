------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with LKQL.Patterns;       use LKQL.Patterns;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;

package body LKQL.Custom_Selectors is

   ----------
   -- Next --
   ----------

   function Next (Iter   : in out Custom_Selector_Iter;
                  Result : out Depth_Node) return Boolean
   is
   begin
      Iter.Eval_Selector;

      if Iter.Next_Values.Is_Empty then
         if Iter.Next_To_Visit.Is_Empty then
            return False;
         else
            return Iter.Next (Result);
         end if;
      end if;

      Result := Iter.Next_Values.First_Element;
      Iter.Next_Values.Delete_First;
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   function Clone (Iter : Custom_Selector_Iter) return Custom_Selector_Iter is
      (Iter);

   -------------------------------
   -- Make_Custom_Selector_Iter --
   -------------------------------

   function Make_Custom_Selector_Iter
     (Ctx                            : Eval_Context;
      Selector                       : Primitive;
      Min_Depth_Expr, Max_Depth_Expr : L.Expr;
      Root                           : AST_Node_Rc) return Custom_Selector_Iter
   is

      Default_Min   : constant Primitive := To_Primitive (1);
      Min_Depth     : constant Integer :=
        Int_Val (Eval_Default (Ctx, Min_Depth_Expr, Default_Min,
                               Expected_Kind => Kind_Int));
      Default_Max   : constant Primitive := To_Primitive (-1);
      Max_Depth     : constant Integer :=
        Int_Val (Eval_Default (Ctx, Max_Depth_Expr, Default_Max,
                               Expected_Kind => Kind_Int));
      Root_Node     : constant Depth_Node :=
        Depth_Node'(0, Root);

      Eval_Ctx      : constant Eval_Context :=
        Eval_Context'(Ctx.Kernel,
                      Eval_Contexts.Environment_Access (Selector.Get.Frame));
   begin
      return Result : Custom_Selector_Iter do
         Result := Custom_Selector_Iter'
           (Eval_Ctx, Selector.Get.Sel_Node,
            Min_Depth, Max_Depth, others => <>);

         Result.Next_To_Visit.Append (Root_Node);

         if Min_Depth < 1 then
            Result.Next_Values.Append (Root_Node);
         end if;
      end return;
   end Make_Custom_Selector_Iter;

   -------------------
   -- Eval_Selector --
   -------------------

   procedure Eval_Selector (Iter : in out Custom_Selector_Iter) is
      use Depth_Node_Lists;
      Position : Cursor := Iter.Next_To_Visit.First;
      Node     : Depth_Node;
   begin
      if not Has_Element (Position) then
         return;
      end if;

      Node := Element (Position);
      Iter.Next_To_Visit.Delete (Position);
      Eval_Selector (Iter, Node);
   end Eval_Selector;

   -------------------
   -- Eval_Selector --
   -------------------

   procedure Eval_Selector (Iter : in out Custom_Selector_Iter;
                            Node : Depth_Node)
   is
      Local_Ctx  : Eval_Context;
      Node_Value : constant Primitive := To_Primitive (Node.Node);
      Dummy  : constant Primitive := To_Primitive (Node.Depth);
      Match_Data : constant Match_Array_Result :=
        Match_Pattern_Array (Iter.Ctx, Iter.Selector.P_Patterns, Node_Value);
   begin
      if Match_Data.Index = 0 then
         return;
      end if;

      Local_Ctx := Iter.Ctx.Create_New_Frame;
      Local_Ctx.Add_Binding ("it", Node_Value);
      Local_Ctx.Add_Binding ("depth", To_Primitive (Node.Depth));

      for E of Iter.Selector.P_Nth_Expressions (Match_Data.Index) loop
         Add_Selector_Expr (Iter, Local_Ctx, Node.Depth, E.As_Selector_Expr);
      end loop;

      Local_Ctx.Release_Current_Frame;
   end Eval_Selector;

   -----------------------
   -- Add_Selector_Expr --
   -----------------------

   procedure Add_Selector_Expr (Iter      : in out Custom_Selector_Iter;
                                Local_Ctx : Eval_Context;
                                Depth     : Natural;
                                Expr      : L.Selector_Expr)
   is
      use type LCO.LKQL_Node_Kind_Type;
      Expr_Value : constant Primitive :=
        (if Expr.F_Expr.Kind = LCO.LKQL_Unpack
         then Eval (Local_Ctx, Expr.F_Expr.As_Unpack.F_Collection_Expr)
         else Eval (Local_Ctx, Expr.F_Expr));
   begin

      if Kind (Expr_Value) = Kind_Node then
         Add_Node (Iter, Depth, Node_Val (Expr_Value), Expr.F_Mode);
      elsif Kind (Expr_Value) in Sequence_Kind and then
        Expr.F_Expr.Kind = LCO.LKQL_Unpack
      then
         for N of List_Val (Expr_Value).Elements loop
            Add_Node (Iter, Depth, Node_Val (N), Expr.F_Mode);
         end loop;
      elsif Kind (Expr_Value) = Kind_Unit then
         return;
      else
         Raise_Invalid_Kind_For_Selector (Local_Ctx, Expr, Expr_Value);
      end if;
   end Add_Selector_Expr;

   --------------
   -- Add_Node --
   --------------

   procedure Add_Node (Iter          : in out Custom_Selector_Iter;
                       Current_Depth : Natural;
                       Node          : AST_Node_Rc;
                       Mode          : L.Selector_Expr_Mode)
   is
      use type LCO.LKQL_Node_Kind_Type;
      Depth_Offset : constant Integer :=
        (if Mode.Kind = LCO.LKQL_Selector_Expr_Mode_Skip then 0 else 1);
      With_Depth : constant Depth_Node :=
        Depth_Node'(Current_Depth + Depth_Offset, Node);
   begin

      if Node.Get.Is_Null_Node or else
        (Iter.Max_Depth >= 0 and then With_Depth.Depth > Iter.Max_Depth)
      then
         return;
      end if;

      --  If the node's depth is too low we want to visit it without adding
      --  it to the list of nodes to be yielded.
      if Mode.Kind /= LCO.LKQL_Selector_Expr_Mode_Skip and
        (Iter.Min_Depth < 0 or else With_Depth.Depth >= Iter.Min_Depth)
      then
         Add_If_Unseen (With_Depth, Iter.Already_Yielded, Iter.Next_Values);
      end if;

      if Mode.Kind /= LCO.LKQL_Selector_Expr_Mode_Default then
         Add_If_Unseen (With_Depth, Iter.Already_Visited, Iter.Next_To_Visit);
      end if;
   end Add_Node;

   -------------------
   -- Add_If_Unseen --
   -------------------

   procedure Add_If_Unseen
     (Node        : Depth_Node;
      Cache       : in out Node_Sets.Set;
      Target_List : out Depth_Node_Lists.List)
   is
   begin
      if Cache.Contains (Node.Node) then
         return;
      end if;

      Cache.Insert (Node.Node);
      Target_List.Append (Node);
   end Add_If_Unseen;

end LKQL.Custom_Selectors;
