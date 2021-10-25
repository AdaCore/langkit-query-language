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

with LKQL.Patterns;          use LKQL.Patterns;
with LKQL.Patterns.Match;    use LKQL.Patterns.Match;
with LKQL.Evaluation;        use LKQL.Evaluation;
with LKQL.Error_Handling;    use LKQL.Error_Handling;
with LKQL.Adaptive_Integers; use LKQL.Adaptive_Integers;
with LKQL.AST_Nodes;

package body LKQL.Custom_Selectors is

   procedure Add_Selector_Expr (Iter         : in out Custom_Selector_Iter;
                                Local_Ctx    : Eval_Context;
                                Depth        : Natural;
                                Expr         : L.Selector_Expr;
                                Cache_Vector : Node_Vector);
   --  Add the result of 'Expr's evaluation to the values produced by the
   --  selector.

   procedure Add_Node (Iter          : in out Custom_Selector_Iter;
                       Current_Depth : Natural;
                       Node          : H.AST_Node_Holder;
                       Mode          : L.Selector_Expr_Mode;
                       Cache_Vector  : Node_Vector);
   --  Add the given node to the values produced by the selector.
   --  The value will be added to 'Next_Values' or 'Next_To_Visit' (or both)
   --  depending on the given mode.

   ----------
   -- Next --
   ----------

   function Next
     (Iter   : in out Custom_Selector_Iter;
      Result : out Depth_Node) return Boolean
   is
   begin
      --  Eval the next selector call
      Iter.Eval_Selector;

      --  If there are no values in the queue
      if Iter.Next_Values.Is_Empty then
         --  Try to compute next values
         if Iter.Next_To_Visit.Is_Empty then
            --  If we end up here, we really have exhausted the selector,
            --  because it means that the call to eval selector didn't add
            --  values to return, nor values to visit.
            return False;
         else
            --  If we're here, there were no values to return, but there sare
            --  till be values to visit: Call Next recursively to eval the
            --  selector call on them.
            return Iter.Next (Result);
         end if;
      end if;

      --  Iterate over the values to return
      loop
         --  If there are no more values, then exit the loop
         exit when Iter.Next_Values.Is_Empty;

         --  Check the next value to return, and verify whether it's within the
         --  depth bounds.
         Result := Iter.Next_Values.First_Element;

         if Iter.Max_Depth >= 0 and then Result.Depth > Iter.Max_Depth then
            --  If it's over the depth bounds, just end the selector evaluation
            --  completely.
            return False;
         elsif Iter.Min_Depth >= 0 and then Result.Depth < Iter.Min_Depth then
            --  If it's below, we need to continue evaluating but just filter
            --  the values: keep going.
            Iter.Next_Values.Delete_First;
         else
            --  It's within bounds, return true
            Iter.Next_Values.Delete_First;
            return True;
         end if;
      end loop;

      --  Here, there might still be values to visit, so call Next recursively.
      return Iter.Next (Result);
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
      Root                           : H.AST_Node_Holder)
      return Custom_Selector_Iter
   is

      Default_Min   : constant Primitive := To_Primitive (1, Ctx.Pool);
      Min_Depth     : constant Integer :=
        +Int_Val (Eval_Default (Ctx, Min_Depth_Expr, Default_Min,
                               Expected_Kind => Kind_Int));
      Default_Max   : constant Primitive := To_Primitive (-1, Ctx.Pool);
      Max_Depth     : constant Integer :=
        +Int_Val (Eval_Default (Ctx, Max_Depth_Expr, Default_Max,
                               Expected_Kind => Kind_Int));
      Root_Node     : constant Depth_Node :=
        Depth_Node'(0, Root);

      Eval_Ctx      : constant Eval_Context :=
        Eval_Context'(Ctx.Kernel,
                      Eval_Contexts.Environment_Access (Selector.Frame));
   begin
      return Result : Custom_Selector_Iter do
         Result := Custom_Selector_Iter'
           (Eval_Ctx, Selector,
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
      Has_Cache : constant Boolean := Iter.Selector.Sel_Cache /= null;
      Cache_Cursor : constant Node_To_Nodes.Cursor :=
        (if Has_Cache
         then Iter.Selector.Sel_Cache.Find (Node.Node)
         else Node_To_Nodes.No_Element);
   begin
      if Node_To_Nodes.Has_Element (Cache_Cursor) then
         declare
            Elements : constant Node_Vector :=
              Node_To_Nodes.Element (Cache_Cursor);
         begin
            for El of Elements.all loop
               Add_Node (Iter, Node.Depth, El.Node, El.Mode, null);
            end loop;
         end;
      else
         declare
            Local_Ctx  : Eval_Context;
            Node_Value : constant Primitive :=
              To_Primitive (Node.Node, Iter.Ctx.Pool);
            Dummy  : constant Primitive :=
              To_Primitive (Node.Depth, Iter.Ctx.Pool);
            Sel_Node : constant L.Selector_Decl := Iter.Selector.Sel_Node;
            Match_Data : constant Match_Array_Result :=
              Match_Pattern_Array (Iter.Ctx, Sel_Node.P_Patterns, Node_Value);
            Cache_Vector : constant Node_Vector :=
              (if Has_Cache then new Nodes_Vectors.Vector else null);
         begin
            if Match_Data.Index = 0 then
               return;
            end if;

            Local_Ctx := Iter.Ctx.Create_New_Frame;
            Local_Ctx.Add_Binding ("this", Node_Value);
            Local_Ctx.Add_Binding
              ("depth", To_Primitive (Node.Depth, Iter.Ctx.Pool));

            for E of Sel_Node.P_Nth_Expressions (Match_Data.Index) loop
               Add_Selector_Expr
                 (Iter, Local_Ctx, Node.Depth,
                  E.As_Selector_Expr, Cache_Vector);
            end loop;

            Local_Ctx.Release_Current_Frame;

            if Has_Cache then
               Iter.Selector.Sel_Cache.Include (Node.Node, Cache_Vector);
            end if;
         end;
      end if;
   end Eval_Selector;

   -----------------------
   -- Add_Selector_Expr --
   -----------------------

   procedure Add_Selector_Expr (Iter         : in out Custom_Selector_Iter;
                                Local_Ctx    : Eval_Context;
                                Depth        : Natural;
                                Expr         : L.Selector_Expr;
                                Cache_Vector : Node_Vector)
   is
      use type LCO.LKQL_Node_Kind_Type;

      Expr_Value : constant Primitive :=
        (if Expr.F_Expr.Kind = LCO.LKQL_Unpack
         then Eval (Local_Ctx, Expr.F_Expr.As_Unpack.F_Collection_Expr)
         else Eval (Local_Ctx, Expr.F_Expr));

   begin
      if Kind (Expr_Value) = Kind_Node then
         Add_Node
           (Iter, Depth, Node_Val (Expr_Value), Expr.F_Mode, Cache_Vector);

      --  TODO: This only handles lists, we should handle any kind of sequence
      --  here.
      elsif Kind (Expr_Value) = Kind_List and then
        Expr.F_Expr.Kind = LCO.LKQL_Unpack
      then
         for N of List_Val (Expr_Value).Elements loop
            Add_Node (Iter, Depth, Node_Val (N), Expr.F_Mode, Cache_Vector);
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

   procedure Add_Node
     (Iter          : in out Custom_Selector_Iter;
      Current_Depth : Natural;
      Node          : H.AST_Node_Holder;
      Mode          : L.Selector_Expr_Mode;
      Cache_Vector  : Node_Vector)
   is
      procedure Add_If_Unseen
        (Node        : Depth_Node;
         Cache       : in out Node_Sets.Set;
         Target_List : out Depth_Node_Lists.List);
      --  Add 'Node' to the target list if it's node value is not already in
      --  the cache, and cache it.

      use type LCO.LKQL_Node_Kind_Type;

      Depth_Offset : constant Integer :=
        (if Mode.Kind = LCO.LKQL_Selector_Expr_Mode_Skip then 0 else 1);

      With_Depth : constant Depth_Node :=
        Depth_Node'(Current_Depth + Depth_Offset, Node);

      -------------------
      -- Add_If_Unseen --
      -------------------

      procedure Add_If_Unseen
        (Node        : Depth_Node;
         Cache       : in out Node_Sets.Set;
         Target_List : out Depth_Node_Lists.List) is
      begin
         if Cache.Contains (Node.Node) then
            return;
         end if;

         Cache.Insert (Node.Node);
         Target_List.Append (Node);
      end Add_If_Unseen;

   begin
      if Node.Unchecked_Get.Is_Null_Node then
         return;
      end if;

      --  If the node's depth is too low we want to visit it without adding
      --  it to the list of nodes to be yielded.
      if Mode.Kind /= LCO.LKQL_Selector_Expr_Mode_Skip then
         if Cache_Vector /= null then
            Cache_Vector.Append ((Node, Mode));
         end if;
         Add_If_Unseen (With_Depth, Iter.Already_Yielded, Iter.Next_Values);
      end if;

      if Mode.Kind /= LCO.LKQL_Selector_Expr_Mode_Default then
         Add_If_Unseen (With_Depth, Iter.Already_Visited, Iter.Next_To_Visit);
      end if;
   end Add_Node;

end LKQL.Custom_Selectors;
