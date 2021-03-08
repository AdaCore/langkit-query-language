with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Assertions;                  use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

with LKQL.Selector_Lists;   use LKQL.Selector_Lists;
with LKQL.AST_Nodes;        use LKQL.AST_Nodes;
with LKQL.Depth_Nodes;      use LKQL.Depth_Nodes;
with LKQL.Custom_Selectors; use LKQL.Custom_Selectors;
with LKQL.Errors;           use LKQL.Errors;
with LKQL.Evaluation;       use LKQL.Evaluation;
with LKQL.Error_Handling;   use LKQL.Error_Handling;
with LKQL.Node_Extensions; use LKQL.Node_Extensions;

package body LKQL.Functions is

   function Eval_Print (Ctx : Eval_Context; Expr : L.Expr) return Primitive;
   --  Evaluate a call to the 'print' built-in function

   function Eval_Debug (Ctx : Eval_Context; Node : L.Expr) return Primitive;
   --  Evaluate a call to the 'debug' built-in function

   function Eval_To_List (Ctx : Eval_Context; Node : L.Expr) return Primitive;
   --  Evaluate a call to the 'debug' built-in function

   function Eval_User_Selector_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Sel  : Primitive) return Primitive;
   --  Eval a call to a selector

   ---------------
   -- Eval_Call --
   ---------------

   function Eval_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive
   is
      Func : Primitive;
   begin
      --  If the call is a built-in function call, then eval this built-in
      --  call.
      if Call.P_Is_Builtin_Call then
         return Eval_Builtin_Call (Ctx, Call);
      end if;

      --  Else, eval the name to fetch the called entity
      Func := Eval (Ctx, Call.F_Name);

      --  Called entity should be a function or a selector
      if Kind (Func) not in Kind_Function | Kind_Selector then
         Raise_Invalid_Type (Ctx, Call.As_LKQL_Node,
                             "function or selector", Func);
      end if;

      --  Call the proper eval sub function depending on the kind of the called
      --  entity.
      if Kind (Func) = Kind_Function then
         return Eval_User_Fun_Call (Ctx, Call, Func);
      elsif Kind (Func) = Kind_Selector then
         return Eval_User_Selector_Call (Ctx, Call, Func);
      end if;

      raise Program_Error with "unreachable";

   end Eval_Call;

   ------------------------
   -- Eval_User_Fun_Call --
   ------------------------

   function Eval_User_Fun_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Func : Primitive) return Primitive
   is

      type Has_Arg_Array is array (Positive range <>) of Boolean;
      --  Array of booleans, used to check whether an arg was passed.

      Def : constant L.Base_Function := Func.Get.Fun_Node;
      Env : constant LKQL.Primitives.Environment_Access :=
        Func.Get.Frame;

      Def_Ext           : constant Ext := Get_Ext (Def);

      Args_Bindings : Environment_Map;

      Has_Arg  : Has_Arg_Array
        (Def.F_Parameters.First_Child_Index
         .. Def.F_Parameters.Last_Child_Index)
        := (others => False);
   begin

      --  Do the argument evaluation and checking in the same pass
      for I in
        Call.F_Arguments.First_Child_Index .. Call.F_Arguments.Last_Child_Index
      loop
         declare
            Arg : constant L.Arg := Call.F_Arguments.Child (I).As_Arg;
            Arg_Name : constant Symbol_Type := Symbol (Arg.P_Name);
         begin
            if Arg_Name /= null then

               --  Named arg: check if the name exists in the definition's
               --  profile.
               declare
                  Cur : constant Params_Maps.Cursor :=
                    Def_Ext.Content.Params.Find (Arg_Name);
               begin
                  if Params_Maps.Has_Element (Cur) then
                     declare
                        FPI      : constant Formal_Param_Info :=
                          Params_Maps.Element (Cur);
                        Dummy    : String_Value_Maps.Cursor;
                        Inserted : Boolean;
                     begin

                        --  All is good, mark the arg as passed and insert the
                        --  value in the args env map.

                        Has_Arg (FPI.Pos) := True;
                        Args_Bindings.Insert
                          (Arg_Name, Eval (Ctx, Arg.P_Expr), Dummy, Inserted);
                        if not Inserted then
                           Raise_Already_Seen_Arg (Ctx, Arg);
                        end if;
                     end;
                  else
                     --  No parameter for this arg: raise
                     Raise_Unknown_Argument (Ctx, Arg.P_Name);
                  end if;
               end;
            else

               --  Positional arg: check if there is an arg at this position.
               if I > Def.P_Arity then

                  --  No arg at this pos: raise
                  Raise_Invalid_Arity (Ctx, Def.P_Arity, Call.F_Arguments);
               else

                  --  All is good, mark the arg as passed and insert the value
                  --  in the args env map.

                  Args_Bindings.Insert
                    (Symbol (Def.F_Parameters.Child (I)
                             .As_Parameter_Decl.P_Identifier),
                     Eval (Ctx, Arg.P_Expr));
                  Has_Arg (I) := True;
               end if;
            end if;
         end;
      end loop;

      --  Second step: check that every arg has been passed, and evaluate
      --  default values for parameters that were passed no value.
      for I in Has_Arg'Range loop
         --  We have no argument at position I
         if not Has_Arg (I) then
            declare
               Param : constant L.Parameter_Decl :=
                 Def.F_Parameters.Child (I).As_Parameter_Decl;
            begin
               --  It could be an arg with a default value ..
               if not Param.F_Default_Expr.Is_Null then

                  --  In that case eval the default value and add it to the
                  --  args map.
                  Args_Bindings.Include
                    (Symbol (Param.P_Identifier),
                     Eval (Ctx, Param.F_Default_Expr));
               else
                  --  But if not, raise
                  Raise_And_Record_Error
                    (Ctx,
                     Make_Eval_Error
                       (Call, "Missing value for param in call"));
               end if;
            end;
         end if;
      end loop;

      declare
         Eval_Ctx : constant Eval_Context :=
           Eval_Context'(Ctx.Kernel, Eval_Contexts.Environment_Access (Env));
      begin
         return Eval
           (Eval_Ctx, Def.F_Body_Expr, Local_Bindings => Args_Bindings);
      end;
   end Eval_User_Fun_Call;

   -----------------------------
   -- Eval_User_Selector_Call --
   -----------------------------

   function Eval_User_Selector_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Sel  : Primitive) return Primitive
   is
      pragma Warnings (Off);
      Def : constant L.Selector_Decl := Sel.Get.Sel_Node;
      Env : constant LKQL.Primitives.Environment_Access :=
        Sel.Get.Frame;
      S_List : Selector_List;
      Eval_Ctx      : constant Eval_Context :=
        Eval_Context'(Ctx.Kernel, Eval_Contexts.Environment_Access (Env));
   begin
      if Call.F_Arguments.Last_Child_Index = 0 then
         Raise_And_Record_Error
           (Ctx,
            Make_Eval_Error
              (Call, "Selector call should have a node argument"));
      end if;

      declare
         Root_Node_Arg : Primitive := Eval
           (Ctx,
            Call.F_Arguments.Child (1).As_Expr_Arg.F_Value_Expr,
            Kind_Node);

         Root          : AST_Node_Rc := Root_Node_Arg.Get.Node_Val;

         Selector_Iterator : constant Depth_Node_Iter_Access :=
           new Depth_Node_Iter'Class'
             (Depth_Node_Iter'Class
                (Make_Custom_Selector_Iter
                   (Ctx, Sel, L.No_Expr, L.No_Expr, Root)));
      begin
         return To_Primitive (Make_Selector_List (Selector_Iterator));
      end;

   end Eval_User_Selector_Call;

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
      elsif Call.F_Name.Text = "list" then
         return Eval_To_List (Ctx, Call.F_Arguments.List_Child (1).P_Expr);
      end if;

      raise Assertion_Error with "should never happen";
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

   ------------------
   -- Eval_To_List --
   ------------------

   function Eval_To_List (Ctx : Eval_Context; Node : L.Expr) return Primitive
   is
      Value : constant Primitive := Eval (Ctx, Node, Kind_Iterator);
   begin
      return To_List (Value.Get.Iter_Val.all);
   end Eval_To_List;

end LKQL.Functions;
