with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

with LKQL.String_Utils; use LKQL.String_Utils;

package body LKQL.Functions is

   -------------------
   -- Eval_Fun_Call --
   -------------------

   function Eval_Fun_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive
   is
      Func : Primitive;
   begin
      if Call.P_Is_Builtin_Call then
         return Eval_Builtin_Call (Ctx, Call);
      end if;

      Func := Eval (Ctx, Call.F_Name, Expected_Kind => Kind_Function);

      return Eval_User_Fun_Call (Ctx, Call, Func.Get.Fun_Node);
   end Eval_Fun_Call;

   ------------------------
   -- Eval_User_Fun_Call --
   ------------------------

   function Eval_User_Fun_Call (Ctx  : Eval_Context;
                                Call : L.Fun_Call;
                                Def  : L.Base_Function) return Primitive
   is
      Resolved_Arguments : constant L.Named_Arg_Array
        := Call.P_Resolved_Arguments (Def);

      Names_Seen         : String_Set;
      --  TODO: This check for names seen could/should be done at the same time
      --  as resolution of arguments probably.
      Expected_Arity : constant Integer := Def.P_Arity;

   begin
      if Resolved_Arguments'Length /= Expected_Arity then
         Raise_Invalid_Arity (Ctx, Expected_Arity, Call.F_Arguments);
      end if;

      for Arg of Call.F_Arguments loop
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

      declare
         Args_Bindings : constant Environment_Map :=
           Eval_Arguments (Ctx, Resolved_Arguments);
         Fun_Ctx       : constant Eval_Context :=
           (if Ctx.Is_Root_Context then Ctx else Ctx.Parent_Context);
      begin
         return Eval
           (Fun_Ctx, Def.F_Body_Expr, Local_Bindings => Args_Bindings);
      end;
   end Eval_User_Fun_Call;

   --------------------
   -- Eval_Arguments --
   --------------------

   function Eval_Arguments
     (Ctx       : Eval_Context;
      Arguments : L.Named_Arg_Array) return Environment_Map
   is
      Args_Bindings : Environment_Map;
   begin
      for Arg of Arguments loop
         declare
            Arg_Name  : constant Unbounded_Text_Type :=
              To_Unbounded_Text (Arg.P_Name.Text);
            Arg_Value : constant Primitive := Eval (Ctx, Arg.P_Expr);
         begin
            Args_Bindings.Insert (Arg_Name, Arg_Value);
         end;
      end loop;

      return Args_Bindings;
   end Eval_Arguments;

   -----------------------
   -- Eval_Builtin_Call --
   -----------------------

   function Eval_Builtin_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive
   is
   begin
      if Call.P_Arity /= 1 then
         Raise_Invalid_Arity (Ctx, 1, Call.F_Arguments);
      end if;

      if Call.F_Name.Text = "print" then
         return Eval_Print (Ctx, Call.F_Arguments.List_Child (1).P_Expr);
      elsif Call.F_Name.Text = "debug" then
         return Eval_Debug (Ctx, Call.F_Arguments.List_Child (1).P_Expr);
      end if;

      Raise_Unknown_Symbol (Ctx, Call.F_Name);
   end Eval_Builtin_Call;

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print (Ctx : Eval_Context; Expr : L.Expr) return Primitive is
   begin
      Display (Eval (Ctx, Expr));
      return Make_Unit_Primitive;
   end Eval_Print;

   ----------------
   -- Eval_Debug --
   ----------------

   function Eval_Debug (Ctx : Eval_Context; Node : L.Expr) return Primitive is
      Code  : constant Text_Type := Node.Text;
      Value : constant Primitive := Eval (Ctx, Node);
      Message : constant Unbounded_Text_Type :=
        Code & " = " & To_Unbounded_Text (Value);
   begin
      Put_Line (Message);
      return Value;
   end Eval_Debug;

end LKQL.Functions;
