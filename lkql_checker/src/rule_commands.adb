with Exec;       use Exec;
with Unit_Utils; use Unit_Utils;

package body Rule_Commands is

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Actual_Kind : Valid_Primitive_Kind;
      Context       : String)
   is
   begin
      if Expected_Kind /= Actual_Kind then
         raise Rule_Error
           with Context & ": expected value of kind " &
           To_String (Expected_Kind) & "but got " & To_String (Actual_Kind);
      end if;
   end Check_Kind;

   -------------------------
   -- Create_Rule_Command --
   -------------------------

   function Create_Rule_Command
     (Name                : Text_Type;
      LKQL_Script_Path    : String;
      Function_Name       : Text_Type := "result";
      Arguments           : Environment_Map := String_Value_Maps.Empty_Map)
      return Rule_Command
   is
      Context : L.Analysis_Context;
      Root    : constant L.LKQL_Node :=
        Make_LKQL_Unit (LKQL_Script_Path, Context).Root;
   begin
      return Rule_Command'
        (Name          => To_Unbounded_Text (Name),
         Function_Name => To_Unbounded_Text (Function_Name),
         Arguments     => Arguments,
         LKQL_Root     => Root,
         LKQL_Context  => Context);
   end Create_Rule_Command;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Self : Rule_Command; Ada_Units : Ada_Unit_Vectors.Vector)
      return Rule_Result
   is
      Flagged : Ada_Node_Vector;
      Ctx     : Eval_Context := Load (Self.LKQL_Root);
   begin
      for Unit of Ada_Units loop
         Ctx.Set_AST_Root (Unit.Root);
         Flagged.Append (Get_Flagged_Nodes (Self, Ctx));
      end loop;

      Free_Eval_Context (Ctx);
      return Make_Rule_Result (Self.Name, Flagged);
   end Evaluate;

   -----------------------
   -- Get_Flagged_Nodes --
   -----------------------

   function Get_Flagged_Nodes
     (Command : Rule_Command; Ctx : Eval_Context) return Ada_Node_Vector
   is
      Result       : Ada_Node_Vector;
      Command_Name : constant String    := To_UTF8 (To_Text (Command.Name));
      Nodes        : constant Primitive :=
        Call_Function (Ctx, Command.Function_Name, Command.Arguments);
   begin
      Check_Kind (Kind_List, Kind (Nodes), "Result of " & Command_Name);

      for N of List_Val (Nodes).Elements loop
         Check_Kind
           (Kind_Node, Kind (N), "Element from the result of " & Command_Name);
         Result.Append (Node_Val (N));
      end loop;

      return Result;
   end Get_Flagged_Nodes;

end Rule_Commands;
