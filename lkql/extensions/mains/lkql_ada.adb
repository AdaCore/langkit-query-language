------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with GNATCOLL.Opt_Parse;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Diagnostics.Output;

with Libadalang.Helpers;  use Libadalang.Helpers;
with Libadalang.Generic_API;

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

procedure Lkql_Ada is

   use LKQL;

   package L renames Liblkqllang.Analysis;

   procedure Job_Setup (Context : App_Job_Context);

   procedure Job_Post_Process
     (Context : App_Job_Context);

   procedure Evaluate
     (Context : Eval_Context; Lkql_Script : L.Lkql_Node);

   package App is new Libadalang.Helpers.App
     (Name               => "lkql_ada_interpreter",
      Description        => "LKQL Ada interpreter",
      Enable_Parallelism => True,
      Job_Post_Process   => Job_Post_Process,
      Job_Setup          => Job_Setup);

   Lkql_Unit           : L.Analysis_Unit;

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

   procedure Job_Setup (Context : App_Job_Context)
   is
   begin
      null;
   end Job_Setup;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (Context : Eval_Context; Lkql_Script : L.Lkql_Node)
   is
      Ignore : Primitive;
   begin
      Ignore := Eval (Context, Lkql_Script);
   exception
      when Stop_Evaluation_Error =>
         pragma Assert (Is_Error (Context.Last_Error),
                        "Stop Evaluation Error raised without adding the " &
                          "error to the evaluation context");

         declare
            N : L.Lkql_Node renames Context.Last_Error.AST_Node;
            D : constant Diagnostic := Langkit_Support.Diagnostics.Create
              (N.Sloc_Range,
               To_Text (Context.Last_Error.Short_Message));
         begin
            Output.Print_Diagnostic
              (D, N.Unit, Simple_Name (N.Unit.Get_Filename));
         end;
   end Evaluate;

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure Job_Post_Process
     (Context : App_Job_Context)
   is
      Roots : LK.Lk_Node_Array
        (Context.Units_Processed.First_Index ..
         Context.Units_Processed.Last_Index);
   begin
      for J in Roots'Range loop
         Roots (J) := Libadalang.Generic_API.To_Generic_Unit
           (Context.Units_Processed (J)).Root;
      end loop;

      Interpreter_Context := Make_Eval_Context
        (Roots,
         Libadalang.Generic_API.Ada_Lang_Id);

      Lkql_Unit := Make_Lkql_Unit
        (Interpreter_Context, To_String (Args.Script_Path.Get));

      Evaluate (Interpreter_Context, Lkql_Unit.Root);
      Interpreter_Context.Free_Eval_Context;
   exception
      when Unit_Creation_Error => null;
   end Job_Post_Process;

begin
   App.Run;
end Lkql_Ada;
