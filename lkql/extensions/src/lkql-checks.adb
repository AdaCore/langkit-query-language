with LKQL.String_Utils;   use LKQL.String_Utils;
with LKQL.Error_Handling; use LKQL.Error_Handling;

with Langkit_Support.Text; use Langkit_Support.Text;

package body LKQL.Checks is

   -----------
   -- Check --
   -----------

   procedure Check (Ctx : Eval_Context; Node : L.LKQL_Node'Class) is
      use type LCO.LKQL_Node_Kind_Type;
   begin
      if Node.Is_Null then
         return;
      end if;

      case Node.Kind is
         when LCO.LKQL_Fun_Call =>
            --  Don't check funcalls that are part of property links
            if Node.Parent.Kind = LCO.LKQL_Property_Link then
               return;
            end if;
            if not Node.As_Fun_Call.P_Is_Builtin_Call then
               Check_Fun_Call (Ctx, Node.As_Fun_Call);
            end if;
         when others =>
            for N of Node.Children loop
               Check (Ctx, N);
            end loop;
      end case;
   end Check;

   --------------------
   -- Check_Fun_Call --
   --------------------

   procedure Check_Fun_Call (Ctx : Eval_Context; Node : L.Fun_Call) is
   begin
      if Node.P_Is_Builtin_Call or else not Node.P_Is_Actual_Call then
         return;
      else
         Check_Fun_Call_Arguments (Ctx, Node);
      end if;
   end Check_Fun_Call;

   --------------------------
   -- Check_Call_Arguments --
   --------------------------

   procedure Check_Fun_Call_Arguments (Ctx : Eval_Context; Node : L.Fun_Call)
   is
      Names_Seen     : String_Set;
      Def            : constant L.Fun_Decl := Node.P_Called_Function;
      Expected_Arity : constant Integer :=
        Node.P_Called_Function.P_Arity;
   begin
      if Node.P_Resolved_Arguments'Length /= Expected_Arity then
         Raise_Invalid_Arity (Ctx, Expected_Arity, Node.F_Arguments);
      end if;

      for Arg of Node.F_Arguments loop
         if Arg.P_Has_Name then
            if not Def.P_Has_Parameter (Arg.P_Name.Text) then
               Raise_Unknown_Argument (Ctx, Arg.P_Name);
            end if;

            if Names_Seen.Contains (To_Unbounded_Text (Arg.P_Name.Text)) then
               Raise_Already_Seen_Arg (Ctx, Arg.As_Named_Arg);
            end if;

            Names_Seen.Insert (To_Unbounded_Text (Arg.P_Name.Text));
         elsif not Names_Seen.Is_Empty then
               Raise_Positionnal_After_Named (Ctx, Arg.As_Expr_Arg);
         end if;
      end loop;
   end Check_Fun_Call_Arguments;

end LKQL.Checks;
