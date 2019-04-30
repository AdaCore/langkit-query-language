with Builtin_Functions;          use Builtin_Functions;
with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Langkit_Support.Text; use Langkit_Support.Text;

package body Functions is

   -------------------
   -- Eval_Fun_Call --
   -------------------

   function Eval_Fun_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive
   is
      Fun_Def : L.Fun_Def;
   begin
      if Is_Builtin_Call (Call) then
         return Eval_Builtin_Call (Ctx, Call);
      end if;

      Fun_Def := Fun_Val (Eval (Ctx, Call.F_Name, Expected_Kind => Kind_Fun));

      return Eval_User_Fun_Call (Ctx, Call, Fun_Def);
   end Eval_Fun_Call;

   ------------------------
   -- Eval_User_Fun_Call --
   ------------------------

   function Eval_User_Fun_Call (Ctx  : Eval_Context;
                                Call : L.Fun_Call;
                                Def  : L.Fun_Def) return Primitive
   is
      Args_Bindings : constant Environment_Map :=
        Eval_Arguments (Ctx, Call.F_Arguments, Def);
      Fun_Ctx       : constant Eval_Context :=
           (if Ctx.Is_Root_Context then Ctx else Ctx.Parent_Context);
   begin
      return Eval
        (Fun_Ctx, Def.F_Body_Expr, Local_Bindings => Args_Bindings);
   end Eval_User_Fun_Call;

   --------------------
   -- Eval_Arguments --
   --------------------

   function Eval_Arguments (Ctx       : Eval_Context;
                            Arguments : L.Arg_List;
                            Def       : L.Fun_Def)
                            return Environment_Map
   is
      Args_Bindings : Environment_Map;
   begin
      Check_Arguments (Ctx, Arguments, Def);

      for I in Arguments.First_Child_Index .. Arguments.Last_Child_Index loop
         declare
            Arg       : constant L.Arg := Arguments.List_Child (I);
            Arg_Name  : constant Unbounded_Text_Type :=
              (if Arg.P_Has_Name
               then To_Unbounded_Text (Arg.P_Name.Text)
               else To_Unbounded_Text (Def.F_Parameters.List_Child (I).Text));
            Arg_Value : constant Primitive := Eval (Ctx, Arg.P_Expr);
         begin
            Args_Bindings.Insert (Arg_Name, Arg_Value);
         end;
      end loop;

      return Args_Bindings;
   end Eval_Arguments;

   ---------------------
   -- Check_Arguments --
   ---------------------

   procedure Check_Arguments (Ctx       : Eval_Context;
                              Arguments : L.Arg_List;
                              Def       : L.Fun_Def)
   is
      Names_Seen : String_Set;
   begin
      if Arguments.Children_Count /= Def.P_Arity then
         Raise_Invalid_Arity (Ctx, Def.P_Arity, Arguments);
      end if;

      for Arg of Arguments loop
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

   end Check_Arguments;

end Functions;
