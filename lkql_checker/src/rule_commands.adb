with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;
with Libadalang.Analysis; use Libadalang.Analysis;
with Exec; use Exec;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.AST_Nodes; use LKQL.AST_Nodes;

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
      Code                : Text_Type := "result()")
      return Rule_Command
   is
      Context : L.Analysis_Context;
      Root    : constant L.LKQL_Node :=
        Make_LKQL_Unit (LKQL_Script_Path, Context).Root;
   begin
      return Rule_Command'
        (Name          => To_Unbounded_Text (Name),
         Code          => To_Unbounded_Text (Code),
         LKQL_Root     => Root,
         LKQL_Context  => Context);
   end Create_Rule_Command;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Self : Rule_Command;
      Unit : LAL.Analysis_Unit) return Message_Vectors.Vector
   is
      Result       : Message_Vectors.Vector;
      Command_Name : constant Text_Type := To_Text (Self.Name);
      Nodes, Dummy : Primitive;
      Ctx     : Eval_Context :=
        Make_Eval_Context (Make_Ada_AST_Node (Unit.Root),
                           Make_Ada_AST_Node (No_Ada_Node));
   begin
      Dummy := Check_And_Eval (Ctx, Self.LKQL_Root);
      Nodes := LKQL_Eval (Ctx, Image (To_Text (Self.Code)), Self.LKQL_Context);
      Check_Kind (Kind_List,
                  Kind (Nodes), "Result of " & To_UTF8 (Command_Name));

      for N of List_Val (Nodes).Elements loop
         Check_Kind
           (Kind_Node, Kind (N), "Element from the result of "
            & To_UTF8 (Command_Name));
         declare
            Wrapped_Node : constant AST_Node_Rc := Node_Val (N);
            Ada_Wrapped_Node : constant Ada_AST_Node :=
              Ada_AST_Node (Wrapped_Node.Unchecked_Get.all);
            Node         : constant Ada_Node := Ada_Wrapped_Node.Node;
         begin
            Result.Append
              (To_Unbounded_Text
                 (Node.Full_Sloc_Image & Command_Name & " - rule violation"));
         end;
      end loop;

      Free_Eval_Context (Ctx);
      return Result;
   end Evaluate;

end Rule_Commands;
