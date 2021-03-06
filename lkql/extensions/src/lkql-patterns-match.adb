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

with LKQL.Patterns.Nodes; use LKQL.Patterns.Nodes;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling;

with Ada.Assertions; use Ada.Assertions;

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
         when LCO.LKQL_Node_Pattern =>
            if not (Kind (Value) = Kind_Node) then
               Raise_Invalid_Kind
                 (Ctx, Pattern.As_LKQL_Node, Kind_Node, Value);
            end if;

            return Match_Node_Pattern
              (Ctx, Pattern.As_Node_Pattern, Node_Val (Value));

         when LCO.LKQL_Universal_Pattern =>
            return Make_Match_Success (Value);

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

end LKQL.Patterns.Match;
