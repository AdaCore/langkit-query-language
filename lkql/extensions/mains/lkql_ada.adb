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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada_AST_Nodes; use Ada_AST_Nodes;

with GNATCOLL.Opt_Parse;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Diagnostics.Output;

with Libadalang.Helpers;  use Libadalang.Helpers;

with Liblkqllang.Analysis;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Errors; use LKQL.Errors;
with Langkit_Support.Text; use Langkit_Support.Text;

with LKQL.Unit_Utils; use LKQL.Unit_Utils;

----------
-- Main --
----------

procedure LKQL_Ada is

   package L renames Liblkqllang.Analysis;

   procedure Job_Setup (Context : App_Job_Context);

   procedure Job_Post_Process
     (Context : App_Job_Context);

   procedure Evaluate
     (Context : Eval_Context; LKQL_Script : L.LKQL_Node);

   package App is new Libadalang.Helpers.App
     (Name               => "lkql_ada_interpreter",
      Description        => "LKQL Ada interpreter",
      Enable_Parallelism => True,
      Job_Post_Process   => Job_Post_Process,
      Job_Setup          => Job_Setup);

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
      --  We use an option rt. a positional arg because we cannot add any more
      --  positional args to the App parser.
   end Args;

   Interpreter_Context : Eval_Context;

   ---------------
   -- Job_Setup --
   ---------------

   procedure Job_Setup (Context : App_Job_Context) is
   begin
      null;
   end Job_Setup;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Context : Eval_Context; LKQL_Script : L.LKQL_Node)
   is
      Ignore : Primitive;
   begin
      Ignore := Eval (Context, LKQL_Script);
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

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure Job_Post_Process (Context : App_Job_Context) is
   begin
      Interpreter_Context := Make_Eval_Context (Context.Units_Processed);
      LKQL_Unit := Make_LKQL_Unit
        (Get_Context (Interpreter_Context.Kernel.all),
         To_String (Args.Script_Path.Get));
      Evaluate (Interpreter_Context, LKQL_Unit.Root);
      Interpreter_Context.Free_Eval_Context;
   exception
      when Unit_Creation_Error => null;
   end Job_Post_Process;

begin
   App.Run;
end LKQL_Ada;
