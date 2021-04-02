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

with LKQL.AST_Nodes;
with LKQL.Patterns.Nodes; use LKQL.Patterns.Nodes;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling;
with LKQL.Node_Extensions;

with Ada.Assertions; use Ada.Assertions;

with GNAT.Regpat;

with Langkit_Support.Text; use Langkit_Support.Text;

package body LKQL.Patterns.Match is

   -------------------------
   -- Match_Pattern_Array --
   -------------------------

   function Match_Pattern_Array (Ctx      : Eval_Context;
                                 Patterns : L.Base_Pattern_Array;
                                 Value    : Primitive)
                                 return Match_Array_Result
   is
      Current_Result : Match_Result;
   begin
      for I in Patterns'Range loop
         Current_Result := Match_Pattern (Ctx, Patterns (I), Value);

         if Current_Result.Is_Success then
            return Match_Array_Result'
              (Current_Result.Matched_Value, I);
         end if;
      end loop;

      return Match_Array_Result'(others => <>);
   end Match_Pattern_Array;

   -------------------
   -- Match_Pattern --
   -------------------

   function Match_Pattern (Ctx     : Eval_Context;
                           Pattern : L.Base_Pattern;
                           Value   : Primitive) return Match_Result
   is
   begin
      return (if Pattern.Kind in LCO.LKQL_Unfiltered_Pattern
              then Match_Unfiltered (Ctx, Pattern.As_Unfiltered_Pattern, Value)
              else Match_Filtered (Ctx, Pattern.As_Filtered_Pattern, Value));
   end Match_Pattern;

   --------------------
   -- Match_Filtered --
   --------------------

   function Match_Filtered (Ctx     : Eval_Context;
                            Pattern : L.Filtered_Pattern;
                            Value   : Primitive) return Match_Result
   is
      Result           : constant Match_Result :=
        Match_Unfiltered (Ctx, Pattern.F_Pattern, Value);
      Predicate_Result : Primitive;
   begin
      if not Result.Is_Success then
         return Match_Failure;
      end if;

      Predicate_Result := Eval (Ctx, Pattern.F_Predicate, Kind_Bool);

      if not Bool_Val (Predicate_Result) then
         return Match_Failure;
      end if;

      return Result;
   end Match_Filtered;

   ----------------------
   -- Match_Unfiltered --
   ----------------------

   function Match_Unfiltered (Ctx     : Eval_Context;
                              Pattern : L.Unfiltered_Pattern;
                              Value   : Primitive) return Match_Result
   is
      (case Pattern.Kind is
          when LCO.LKQL_Value_Pattern =>
             Match_Value (Ctx, Pattern.As_Value_Pattern, Value),
          when LCO.LKQL_Binding_Pattern =>
             Match_Binding (Ctx, Pattern.As_Binding_Pattern, Value),
          when others =>
             raise Assertion_Error with
               "Not an unfiltered pattern kind: " & L.Kind_Name (Pattern));

   -----------------
   -- Match_Value --
   -----------------

   function Match_Value (Ctx     : Eval_Context;
                         Pattern : L.Value_Pattern;
                         Value   : Primitive) return Match_Result
   is
      use LKQL.Error_Handling;
   begin
      case Pattern.Kind is
         when LCO.LKQL_Paren_Pattern =>
            return Match_Pattern
              (Ctx, Pattern.As_Paren_Pattern.F_Pattern, Value);

         when LCO.LKQL_Or_Pattern =>
            declare
               Or_Pat   : constant L.Or_Pattern := Pattern.As_Or_Pattern;
               Left_Res : constant Match_Result :=
                  Match_Pattern (Ctx, Or_Pat.F_Left, Value);
            begin
               if Left_Res.Is_Success then
                  return Left_Res;
               else
                  return Match_Pattern (Ctx, Or_Pat.F_Right, Value);
               end if;
            end;

         when LCO.LKQL_Not_Pattern =>
            declare
               Res : constant Match_Result :=
                  Match_Value (Ctx, Pattern.As_Not_Pattern.F_Pattern, Value);
            begin
               if Res.Is_Success then
                  return Match_Failure;
               else
                  return Make_Match_Success (Value);
               end if;
            end;

         when LCO.LKQL_Node_Pattern =>
            if not (Kind (Value) = Kind_Node) then
               Raise_Invalid_Kind
                 (Ctx, Pattern.As_LKQL_Node, Kind_Node, Value);
            end if;

            return Match_Node_Pattern
              (Ctx, Pattern.As_Node_Pattern, Node_Val (Value));

         when LCO.LKQL_Universal_Pattern =>
            return Make_Match_Success (Value);

         when LCO.LKQL_Regex_Pattern =>
            return Match_Regex
              (Ctx, Pattern.As_Regex_Pattern, Value);

         when LCO.LKQL_Null_Pattern =>
            if Value.Get.Node_Val.Get.Is_Null_Node then
               return Make_Match_Success (Value);
            else
               return Match_Failure;
            end if;

         when others =>
            raise Assertion_Error with
              "Invalid pattern kind: " & L.Kind_Name (Pattern);
      end case;
   end Match_Value;

   -------------------
   -- Match_Binding --
   -------------------

   function Match_Binding (Ctx     : Eval_Context;
                           Pattern : L.Binding_Pattern;
                           Value   : Primitive) return Match_Result
   is
      Binding_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Pattern.F_Binding.Text);

      Binding_Match : Match_Result;
   begin
      Ctx.Add_Binding (To_Text (Binding_Name), Value);
      Binding_Match := Make_Match_Success (Value);

      if Match_Value (Ctx, Pattern.F_Value_Pattern, Value).Is_Success
      then
         return Binding_Match;
      else
         return Match_Failure;
      end if;
   end Match_Binding;

   -----------------
   -- Match_Regex --
   -----------------

   function Match_Regex (Ctx     : Eval_Context;
                         Pattern : L.Regex_Pattern;
                         Value   : Primitive) return Match_Result
   is
      pragma Unreferenced (Ctx);

      use LKQL.AST_Nodes;
      use LKQL.Node_Extensions;

      Pat_Ext : constant Ext := Get_Ext (Pattern);
   begin
      case Kind (Value) is
         when Kind_Node =>
            declare
               Node : AST_Node'Class := Node_Val (Value).Unchecked_Get.all;
            begin
               if not Node.Is_Null_Node
                  and then GNAT.Regpat.Match
                    (Pat_Ext.Content.Compiled_Pattern.all,
                     To_UTF8 (Node.Text))
               then
                  return Make_Match_Success (Value);
               end if;
            end;
         when Kind_Str =>
            if GNAT.Regpat.Match
                 (Pat_Ext.Content.Compiled_Pattern.all,
                  To_UTF8 (To_Text (Str_Val (Value))))
            then
               return Make_Match_Success (Value);
            end if;
         when others =>
            null;
      end case;
      return Match_Failure;
   end Match_Regex;
end LKQL.Patterns.Match;
