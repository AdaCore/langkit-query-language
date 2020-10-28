with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada_AST_Nodes; use Ada_AST_Nodes;

with GNAT.OS_Lib;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Diagnostics.Output;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

with Liblkqllang.Analysis;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Errors; use LKQL.Errors;
with Langkit_Support.Text; use Langkit_Support.Text;

----------
-- Main --
----------

procedure LKQL_Ada is

   package L renames Liblkqllang.Analysis;

   procedure Process_Unit
     (Context : App_Job_Context; Unit : Analysis_Unit);

   function Make_LKQL_Unit (Script_Path : String) return L.Analysis_Unit;

   procedure App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array);

   procedure Evaluate
     (Context : Eval_Context; LKQL_Script : L.LKQL_Node);

   package App is new Libadalang.Helpers.App
     (Name               => "lkql_ada_interpreter",
      Description        => "LKQL Ada interpreter",
      Enable_Parallelism => True,
      Process_Unit       => Process_Unit,
      App_Setup          => App_Setup);

   function Make_LKQL_Unit (Script_Path : String) return L.Analysis_Unit is
      Context : constant L.Analysis_Context := L.Create_Context;
      Unit    : constant L.Analysis_Unit :=
        Context.Get_From_File (Script_Path);
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      return Unit;
   end Make_LKQL_Unit;

   LKQL_Unit           : L.Analysis_Unit;

   package Args is
      use GNATCOLL.Opt_Parse;

      package Script_Path is new Parse_Option
        (Parser   => App.Args.Parser,
         Long     => "--script-path",
         Short    => "-S",
         Arg_Type => Unbounded_String,
         Help     => "Path of the LKQL script to evaluate",
         Default_Val => Null_Unbounded_String);
      --  We use an option rt. a positional arg because we cannot add anymore
      --  positional args to the App parser.

      package Recovery is new Parse_Flag
        (Parser => App.Args.Parser,
         Long   => "--recovery",
         Short  => "-r",
         Help   => "Path of the LKQL script to evaluate");

   end Args;

   ---------------
   -- App_Setup --
   ---------------

   procedure App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Jobs, Context);
   begin
      LKQL_Unit := Make_LKQL_Unit (To_String (Args.Script_Path.Get));
   end App_Setup;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Context : Eval_Context; LKQL_Script : L.LKQL_Node)
   is
      Ignore : Primitive;
   begin
      Ignore := Check_And_Eval (Context, LKQL_Script);
   exception
      when Stop_Evaluation_Error =>
         pragma Assert (Is_Error (Context.Last_Error),
                        "Stop Evaluation Error raised without adding the " &
                          "error to the evaluation context");

         if not Context.Error_Recovery_Enabled then
            declare
               N : L.LKQL_Node renames Context.Last_Error.AST_Node;
               D : constant Diagnostic := Langkit_Support.Diagnostics.Create
                 (N.Sloc_Range,
                  To_Text (Context.Last_Error.Short_Message));
            begin
               Output.Print_Diagnostic
                 (D, N.Unit, Simple_Name (N.Unit.Get_Filename));
            end;
         end if;
   end Evaluate;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Context : App_Job_Context; Unit : Analysis_Unit)
   is
      pragma Unreferenced (Context);
      Interpreter_Context : Eval_Context;

   begin
      Interpreter_Context :=
        Make_Eval_Context (Make_Ada_AST_Node (Unit.Root),
                           Make_Ada_AST_Node (No_Ada_Node),
                           Err_Recovery => Args.Recovery.Get);
      Put_Line (Ada.Directories.Simple_Name (Unit.Get_Filename));
      Evaluate (Interpreter_Context, LKQL_Unit.Root);
      Interpreter_Context.Free_Eval_Context;
   end Process_Unit;

begin
   App.Run;
end LKQL_Ada;
