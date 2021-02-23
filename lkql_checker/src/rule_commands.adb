with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;
with Exec; use Exec;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.AST_Nodes; use LKQL.AST_Nodes;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Liblkqllang.Common;
with Liblkqllang.Iterators; use Liblkqllang.Iterators;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Rule_Commands is

   package LCO renames Liblkqllang.Common;

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

   function Create_Rule_Command (LKQL_File_Path : String) return Rule_Command
   is
      Context : L.Analysis_Context;
      Root    : constant L.LKQL_Node :=
        Make_LKQL_Unit (LKQL_File_Path, Context).Root;
      Check_Annotation : constant L.Decl_Annotation :=
        Find_First
          (Root, Kind_Is (LCO.LKQL_Decl_Annotation)).As_Decl_Annotation;
   begin
      if Check_Annotation.Is_Null
        or else Check_Annotation.F_Name.Text not in "check" | "node_check"
      then
         raise Rule_Error
           with "No @check or @node_check annotated function in "
           & LKQL_File_Path;
      end if;

      declare
         Fn   : constant L.Fun_Decl := Check_Annotation.Parent.As_Fun_Decl;
         Name : constant Text_Type := Fn.F_Name.Text;
      begin
         return Rule_Command'
           (Name          => To_Unbounded_Text (To_Lower (Name)),
            LKQL_Root     => Root,
            LKQL_Context  => Context,
            Rule_Args     => <>,
            Is_Node_Check => Check_Annotation.F_Name.Text = "node_check",
            Code          => <>);
      end;
   end Create_Rule_Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Self : in out Rule_Command) is
      Code : Unbounded_Text_Type;
   begin
      --  Create the code snippet that will be passed to LKQL_Eval, along with
      --  the optional arguments passed to the rule via the command line.

      Append (Code, To_Text (Self.Name));
      Append (Code, "(");
      for I in Self.Rule_Args.First_Index .. Self.Rule_Args.Last_Index loop
         Append (Code,
                 To_Text (Self.Rule_Args (I).Name)
                 & "="
                 & To_Text (Self.Rule_Args (I).Value));
         if I < Self.Rule_Args.Last_Index then
            Append (Code, ", ");
         end if;
      end loop;
      Append (Code, ")");

      Self.Code :=
        Make_LKQL_Unit_From_Code
          (Self.LKQL_Context, Image (To_Text (Code))).Root;

   end Prepare;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Self : Rule_Command;
      Ctx  : Eval_Context) return Eval_Diagnostic_Vectors.Vector
   is
      Result       : Eval_Diagnostic_Vectors.Vector;
      Command_Name : constant Text_Type := To_Text (Self.Name);
      Nodes, Dummy : Primitive;
      Code         : Unbounded_Text_Type;
   begin

      --  Eval the rule's code (which should contain only definitions)
      Dummy := Eval (Ctx, Self.LKQL_Root);

      --  Eval the call to the check function
      Nodes := LKQL_Eval (Ctx, Image (To_Text (Code)), Self.LKQL_Context);

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
            Node         : Ada_Node := Ada_Wrapped_Node.Node;
         begin
            if Node.Kind in Ada_Basic_Decl then
               Node := Node.As_Basic_Decl.P_Defining_Name.As_Ada_Node;
            end if;

            Result.Append
              (Eval_Diagnostic'
                 (Diagnostic'
                      (Node.Sloc_Range,
                       To_Unbounded_Text (Command_Name & " - rule violation")),
                  Node.Unit));
         end;
      end loop;

      return Result;
   end Evaluate;

end Rule_Commands;
