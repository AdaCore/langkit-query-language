with Patterns.Nodes;         use Patterns.Nodes;
with Interpreter.Evaluation; use Interpreter.Evaluation;

with Ada.Assertions; use Ada.Assertions;

with Langkit_Support.Text; use Langkit_Support.Text;

package body Patterns.Match is

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

         if Current_Result.Success then
            return Match_Array_Result'(I, Current_Result.Bindings);
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
      if not Result.Success then
         return Match_Failure;
      end if;

      Predicate_Result := Eval (Ctx, Pattern.F_Predicate, Kind_Bool,
                                Local_Bindings => Result.Bindings);

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
             Match_Binding (Pattern.As_Binding_Pattern, Value),
          when LCO.LKQL_Full_Pattern =>
             Match_Full (Ctx, Pattern.As_Full_Pattern, Value),
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
     (case Pattern.Kind is
         when LCO.LKQL_Node_Pattern =>
            Match_Node (Ctx, Pattern.As_Node_Pattern, Value),
         when LCO.LKQL_Universal_Pattern =>
            Make_Match_Success,
         when others =>
            raise Assertion_Error with
              "Cannot match values of kind: " & Kind_Name (Value));

   -------------------
   -- Match_Binding --
   -------------------

   function Match_Binding (Pattern : L.Binding_Pattern;
                           Value   : Primitive) return Match_Result
   is
      Bindings     : Environment_Map;
      Binding_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Pattern.F_Binding.Text);
   begin
      Bindings.Insert (Binding_Name, Value);
      return Match_Result'(True, Bindings);
   end Match_Binding;

   ----------------
   -- Match_Full --
   ----------------

   function Match_Full (Ctx     : Eval_Context;
                        Pattern : L.Full_Pattern;
                        Value   : Primitive) return Match_Result
   is
     (if Match_Value (Ctx, Pattern.F_Value_Pattern, Value).Success
      then Match_Binding (Pattern.As_Binding_Pattern, Value)
      else Match_Failure);

end Patterns.Match;
